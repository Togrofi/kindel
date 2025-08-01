;;; kindel.el --- Email tagging and HTML body extraction for notmuch -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Charlie <charlie@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (notmuch "0.32"))
;; Keywords: mail, notmuch, html, automation
;; URL: https://github.com/charlie/kindel

;;; Commentary:

;; Kindel provides functions to search for tagged emails in notmuch and
;; extract their HTML body content for processing by other Emacs functions.
;;
;; Basic usage:
;;
;;   ;; Tag emails from a sender for processing
;;   (kindel-tag-emails-for-processing "sender@example.com" "my-tag")
;;
;;   ;; Get HTML bodies from tagged emails
;;   (let ((html-bodies (kindel-process-tagged-emails "my-tag")))
;;     (dolist (html html-bodies)
;;       (your-processing-function html)))
;;
;;   ;; Use automation hooks
;;   (add-hook 'kindel-html-body-hook #'your-processing-function)
;;   ;; Enable auto-processing (optional):
;;   (kindel-setup-auto-processing)
;;   ;; Or call manually:
;;   (kindel-process-new-emails)
;;
;; Main functions:
;; - `kindel-process-tagged-emails': Get HTML bodies from tagged emails
;; - `kindel-tag-emails-for-processing': Tag emails from sender
;; - `kindel-show-tagged-emails': Show tagged emails in notmuch
;; - `kindel-process-new-emails': Process emails and run hooks

;;; Code:

(require 'notmuch)
(require 'json)

;;; Configuration

(defcustom kindel-default-tag "kindel"
  "Default tag to apply to emails for processing."
  :type 'string
  :group 'kindel)

(defcustom kindel-processing-tag "kindel-process"
  "Tag applied to emails that need processing."
  :type 'string
  :group 'kindel)

(defcustom kindel-link-text-pattern "View"
  "Pattern to match in link visible text when extracting links from HTML.
Can be a string for exact match or a regexp for pattern matching."
  :type 'string
  :group 'kindel)

(defcustom kindel-auto-download-enabled t
  "Enable automatic downloading of text files from email links."
  :type 'boolean
  :group 'kindel)

(defcustom kindel-download-url-patterns
  '("\\bhttps?://[^[:space:]]+\\.txt\\b"
    "\\bhttps?://[^[:space:]]+/download\\b"
    "\\bhttps?://[^[:space:]]+\\.log\\b")
  "Regex patterns for URLs that should be auto-downloaded.
URLs matching these patterns will be automatically downloaded and
their content passed to text processors."
  :type '(repeat string)
  :group 'kindel)

(defcustom kindel-download-timeout 10
  "Timeout in seconds for HTTP downloads."
  :type 'integer
  :group 'kindel)

(defcustom kindel-text-processors nil
  "Functions to process downloaded text content.
Each function receives (text url message-id) as arguments."
  :type '(repeat function)
  :group 'kindel)

;;; Core Functions

(defun kindel-get-tagged-emails (tag)
  "Get all message IDs for emails with TAG."
  (condition-case err
      (notmuch-call-notmuch-sexp
       "search"
       "--format=sexp"
       "--output=messages"
       (format "tag:%s" tag))
    (error
     (message "Kindel error getting tagged emails: %s" (error-message-string err))
     nil)))

(defun kindel-get-message-body-html (message-id)
  "Extract HTML body content from MESSAGE-ID."
  (condition-case err
      (let* ((message-data (notmuch-call-notmuch-sexp
                           "show" "--format=sexp" message-id))
             (message-part (car message-data))
             (body-parts (cdr (assq 'body message-part))))
        (kindel--extract-html-from-parts body-parts))
    (error
     (message "Kindel error getting message body: %s" (error-message-string err))
     nil)))

(defun kindel--extract-html-from-parts (parts)
  "Recursively extract HTML content from email PARTS."
  (when parts
    (let ((html-content ""))
      (dolist (part parts)
        (let ((content-type (cdr (assq 'content-type part)))
              (content (cdr (assq 'content part))))
          (cond
           ((and content-type (string-match-p "text/html" content-type))
            (setq html-content (concat html-content content)))
           ((assq 'body part)
            (let ((nested-html (kindel--extract-html-from-parts (cdr (assq 'body part)))))
              (when nested-html
                (setq html-content (concat html-content nested-html))))))))
      (if (string-empty-p html-content) nil html-content))))

(defun kindel-extract-links-with-text (html-content &optional pattern)
  "Extract links from HTML-CONTENT where visible text matches PATTERN.
PATTERN defaults to `kindel-link-text-pattern' if not provided.
Returns a list of plists with :url and :text keys."
  (let ((pattern (or pattern kindel-link-text-pattern))
        (links '()))
    (when html-content
      ;; Use regex to find <a> tags with href and extract text content
      (let ((case-fold-search t)) ; Case-insensitive matching
        (with-temp-buffer
          (insert html-content)
          (goto-char (point-min))
          (while (re-search-forward "<a[^>]+href=[\"']\\([^\"']+\\)[\"'][^>]*>\\([^<]*\\)</a>" nil t)
            (let ((url (match-string 1))
                  (text (match-string 2)))
              ;; Clean up the text (remove extra whitespace, decode entities)
              (setq text (replace-regexp-in-string "[\t\n\r ]+" " " text))
              (setq text (string-trim text))
              ;; Check if text matches the pattern
              (when (and text 
                        (not (string-empty-p text))
                        (or (string-match-p pattern text)
                            (string= pattern text)))
                (push (list :url url :text text) links)))))))
    (nreverse links)))

;;; HTTP Download Functions

(defun kindel-extract-download-urls (html-content)
  "Extract URLs from HTML-CONTENT that match download patterns."
  (let ((urls '()))
    (when html-content
      (dolist (pattern kindel-download-url-patterns)
        (let ((case-fold-search t))
          (with-temp-buffer
            (insert html-content)
            (goto-char (point-min))
            (while (re-search-forward pattern nil t)
              (let ((url (match-string 0)))
                (unless (member url urls)
                  (push url urls))))))))
    (nreverse urls)))

(defun kindel-download-text-content (url message-id)
  "Download text content from URL asynchronously and process it."
  (condition-case err
      (url-retrieve url
                    `(lambda (status)
                       (kindel--handle-download-response status ,url ,message-id))
                    nil t kindel-download-timeout)
    (error
     (message "Kindel download error for %s: %s" url (error-message-string err)))))

(defun kindel--handle-download-response (status url message-id)
  "Handle HTTP response from download and stream content to processors."
  (condition-case err
      (if (plist-get status :error)
          (message "Kindel download failed for %s: %s" url (plist-get status :error))
        (progn
          (set-buffer-multibyte t)
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t)
            (let ((content (buffer-substring-no-properties (point) (point-max))))
              (when (and content (not (string-empty-p content)))
                (message "Kindel downloaded %d chars from %s" (length content) url)
                (kindel--stream-text-to-processors content url message-id))))))
    (error
     (message "Kindel response handling error for %s: %s" url (error-message-string err))))
  (kill-buffer))

(defun kindel--stream-text-to-processors (text url message-id)
  "Stream downloaded text content to registered processors and hooks."
  ;; Call registered processor functions
  (dolist (processor kindel-text-processors)
    (condition-case err
        (funcall processor text url message-id)
      (error
       (message "Kindel processor %s failed: %s" processor (error-message-string err)))))
  ;; Run hook with text content
  (run-hook-with-args 'kindel-downloaded-text-hook text url message-id))

(defun kindel-process-download-links (html-content message-id)
  "Extract and download links from HTML-CONTENT for MESSAGE-ID."
  (let ((urls (kindel-extract-download-urls html-content)))
    (when urls
      (message "Kindel: Found %d download URLs in message %s" (length urls) message-id)
      (dolist (url urls)
        (kindel-download-text-content url message-id)))))

(defun kindel-process-tagged-emails (tag)
  "Return list of HTML bodies from all emails with TAG."
  (let ((message-ids (kindel-get-tagged-emails tag))
        (html-bodies '()))
    (dolist (msg-id message-ids)
      (let ((html-body (kindel-get-message-body-html msg-id)))
        (when html-body
          (push html-body html-bodies))))
    (nreverse html-bodies)))

(defun kindel-tag-emails-for-processing (from-address tag)
  "Tag all emails from FROM-ADDRESS with TAG for processing."
  (condition-case err
      (notmuch-call-notmuch-process
       "tag" (format "+%s" tag) "--"
       (format "from:%s" from-address))
    (error
     (message "Kindel error tagging emails: %s" (error-message-string err))
     nil)))

;;; Interactive Functions

;;;###autoload
(defun kindel-show-tagged-emails (tag)
  "Show all emails with TAG in notmuch search view."
  (interactive (list (read-string "Tag: " kindel-default-tag)))
  (notmuch-search (format "tag:%s" tag)))

;;;###autoload
(defun kindel-show-links-from-tagged-emails (tag &optional pattern)
  "Show links with matching text from emails with TAG.
PATTERN defaults to `kindel-link-text-pattern' if not provided."
  (interactive (list (read-string "Tag: " kindel-default-tag)
                     (read-string (format "Link text pattern (default: %s): " 
                                         kindel-link-text-pattern)
                                 nil nil kindel-link-text-pattern)))
  (let ((links (kindel-extract-links-from-tagged-emails tag pattern)))
    (if links
        (with-output-to-temp-buffer "*Kindel Links*"
          (princ (format "Links from emails tagged '%s' with text matching '%s':\n\n" tag pattern))
          (dolist (link links)
            (princ (format "Text: %s\nURL: %s\nMessage: %s\n\n" 
                          (plist-get link :text)
                          (plist-get link :url)
                          (plist-get link :message-id)))))
      (message "No links found matching pattern '%s' in emails tagged '%s'" pattern tag))))

;;; Automation Hooks

;;;###autoload
(defun kindel-process-new-emails ()
  "Process newly tagged emails automatically."
  (interactive)
  (let ((message-ids (kindel-get-tagged-emails kindel-processing-tag)))
    (dolist (msg-id message-ids)
      (let ((html-body (kindel-get-message-body-html msg-id)))
        (when html-body
          ;; Original HTML processing hook (backward compatibility)
          (run-hook-with-args 'kindel-html-body-hook (list html-body))
          ;; New: Auto-download processing
          (when kindel-auto-download-enabled
            (kindel-process-download-links html-body msg-id)))))))

(defcustom kindel-html-body-hook nil
  "Hook run with HTML bodies from processed emails.
Functions in this hook receive a list of HTML body strings."
  :type 'hook
  :group 'kindel)

(defcustom kindel-downloaded-text-hook nil
  "Hook run with downloaded text content from email links.
Functions in this hook receive (text url message-id) as arguments."
  :type 'hook
  :group 'kindel)

;;; Hook Integration

(defun kindel-setup-auto-processing ()
  "Set up automatic processing after notmuch tagging operations.
Add this to your init.el if you want automatic processing:
  (with-eval-after-load \\='kindel (kindel-setup-auto-processing))"
  (interactive)
  (add-hook 'notmuch-after-tag-hook #'kindel-process-new-emails)
  (message "Kindel auto-processing enabled"))

;;; Utility Functions

(defun kindel-register-text-processor (processor-func)
  "Register PROCESSOR-FUNC to process downloaded text content.
PROCESSOR-FUNC should accept (text url message-id) as arguments."
  (add-to-list 'kindel-text-processors processor-func)
  (message "Kindel: Registered text processor %s" processor-func))

(defun kindel-unregister-text-processor (processor-func)
  "Unregister PROCESSOR-FUNC from text processing."
  (setq kindel-text-processors (remove processor-func kindel-text-processors))
  (message "Kindel: Unregistered text processor %s" processor-func))

(defun kindel-extract-links-from-tagged-emails (tag &optional pattern)
  "Extract links with matching text from all emails with TAG.
PATTERN defaults to `kindel-link-text-pattern' if not provided.
Returns a list of plists with :url, :text, and :message-id keys."
  (let ((message-ids (kindel-get-tagged-emails tag))
        (all-links '()))
    (dolist (msg-id message-ids)
      (let ((html-body (kindel-get-message-body-html msg-id)))
        (when html-body
          (let ((links (kindel-extract-links-with-text html-body pattern)))
            (dolist (link links)
              (push (append link (list :message-id msg-id)) all-links))))))
    (nreverse all-links)))

(defun kindel-count-tagged-emails (tag)
  "Count emails with TAG."
  (length (kindel-get-tagged-emails tag)))

(defun kindel-remove-processing-tag (tag)
  "Remove processing TAG from all emails."
  (condition-case err
      (notmuch-call-notmuch-process
       "tag" (format "-%s" tag) "--"
       (format "tag:%s" tag))
    (error
     (message "Kindel error removing tag: %s" (error-message-string err))
     nil)))

(provide 'kindel)

;;; kindel.el ends here
