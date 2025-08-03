;;; kindel.el --- Email tagging and HTML body extraction for notmuch -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Charlie
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (notmuch "0.32"))
;; Keywords: mail, notmuch, html, automation
;; URL: https://github.com/charlie/kindel

;;; Commentary:

;; Kindel provides functions to search for tagged emails in notmuch and
;; extract their HTML body content for processing by other Emacs functions.
;;
;; Basic usage:
;;   (kindel-process-new-emails)
;;
;; Main functions:
;; - `kindel-process-new-emails': Process emails and run hooks

;;; Code:

(require 'notmuch)
(require 'json)
(require 'qp)
(require 'seq)

;;; Coniguration

(defgroup kindel nil
  "Email processing with notmuch and URL extraction."
  :group 'mail
  :prefix "kindel-")

(defcustom kindel-processing-tag "kindel"
  "Tag applied to emails that need processing."
  :type 'string
  :group 'kindel)

(defcustom kindel-processed-tag "kindel-processed"
  "Tag applied to emails after successful processing."
  :type 'string
  :group 'kindel)

(defcustom kindel-link-text-pattern "View"
  "Pattern to match in link visible text when extracting links from HTML.
Can be a string for exact match or a regexp for pattern matching."
  :type 'string
  :group 'kindel)

(defcustom kindel-download-timeout 10
  "Timeout in seconds for HTTP downloads."
  :type 'integer
  :group 'kindel)

(defun kindel-save-to-file (text url message-id)
  "Save downloaded text content to a file.
Files are saved as ~/kindel-downloads/MESSAGE-ID-URL-HASH-TIMESTAMP.txt"
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (url-hash (format "%x" (sxhash url)))
         (filename (format "%s-%s-%s.txt" message-id url-hash timestamp))
         (dir (expand-file-name "~/kindel-downloads"))
         (filepath (expand-file-name filename dir)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (with-temp-file filepath
      (insert text))
    (message "Kindel saved %d chars to %s" (length text) filepath)))

(defcustom kindel-text-processors (list #'kindel-save-to-file)
  "Functions to process downloaded text content.
Each function receives (text url message-id) as arguments."
  :type '(repeat function)
  :group 'kindel)



;;; Core Functions

(defun kindel--update-message-tags (message-id)
  "Update tags for MESSAGE-ID after successful processing.
Removes the processing tag and adds the processed tag."
  (condition-case err
      (progn
        (notmuch-command 
         "tag" 
         (format "-%s" kindel-processing-tag)
         (format "+%s" kindel-processed-tag)
         (format "id:%s" message-id))
        (message "Kindel: Updated tags for %s" message-id)
        t)
    (error
     (message "Kindel error updating tags for %s: %s" 
              message-id (error-message-string err))
     nil)))

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

(defun kindel--get-message-content (message-id)
  "Get the complete raw message content from MESSAGE-ID."
  (condition-case err
      (notmuch-command-to-string "show" "--format=raw" (format "id:%s" message-id))
    (error
     (message "Kindel error getting message content: %s" (error-message-string err))
     nil)))

(defun kindel--decode-message (raw-message)
  "Decode quoted-printable message, handling soft line breaks."
  (let ((no-soft-breaks (replace-regexp-in-string "=\n" "" raw-message)))
    (quoted-printable-decode-string no-soft-breaks)))

(defun kindel--extract-urls-from-html (decoded-message)
  "Extract all URLs from href attributes in decoded HTML message."
  (let ((href-regex "href=\"\\([^\"]+\\)\"")
        (start 0)
        (urls '()))
    (while (string-match href-regex decoded-message start)
      (let ((url (match-string 1 decoded-message)))
        (when (and url (not (string-empty-p url)))
          (push url urls)))
      (setq start (match-end 0)))
    (nreverse urls)))

(defun kindel-decode-and-extract-urls (raw-message)
  "Decode quoted-printable message and extract URLs from href attributes.
First handles quoted-printable soft line breaks, then decodes and extracts URLs."
  (when raw-message
    (let ((decoded-message (kindel--decode-message raw-message)))
      (kindel--extract-urls-from-html decoded-message))))

;;; HTTP Download Functions
(defun kindel--download-text-content (url message-id completion-callback)
  "Download text content from URL asynchronously and call COMPLETION-CALLBACK when done."
  (condition-case err
      (url-retrieve url
                    (lambda (status)
                      (kindel--handle-download-response status url message-id completion-callback))
                    nil t kindel-download-timeout)
    (error
     (message "Kindel network error for %s: %s" url (error-message-string err))
     (funcall completion-callback nil))))

(defun kindel--extract-response-content ()
  "Extract content from HTTP response buffer after headers."
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (when (re-search-forward "^$" nil t)
    (buffer-substring-no-properties (point) (point-max))))

(defun kindel--handle-download-response (status url message-id completion-callback)
  "Handle HTTP response from download and call COMPLETION-CALLBACK with success status."
  (condition-case err
      (if (plist-get status :error)
          (progn
            (message "Kindel HTTP error for %s: %s" url (plist-get status :error))
            (funcall completion-callback nil))
        (let ((content (kindel--extract-response-content)))
          (if (and content (not (string-empty-p content)))
              (progn
                (message "Kindel downloaded %d chars from %s" (length content) url)
                (let ((success (kindel--run-processors content url message-id)))
                  (funcall completion-callback success)))
            (funcall completion-callback nil))))
    (error
     (message "Kindel response handling error for %s: %s" url (error-message-string err))
     (funcall completion-callback nil)))
  (kill-buffer))

(defun kindel--run-processors (text url message-id)
  "Run all registered processors on TEXT. Return t if all succeed, nil otherwise."
  (seq-every-p
   (lambda (processor)
     (condition-case err
         (progn (funcall processor text url message-id) t)
       (error
        (message "Kindel processor %s failed: %s" processor (error-message-string err))
        nil)))
   kindel-text-processors))


;;; Interactive Functions

(defun kindel--process-message-urls (msg-id urls)
  "Process URLs for message MSG-ID with simple counter-based coordination."
  (if (null urls)
      (progn
        (message "Kindel: No URLs found in message %s" msg-id)
        (kindel--update-message-tags msg-id))
    (let ((remaining (length urls))
          (any-failed nil))
      (dolist (url urls)
        (message "Kindel downloading from: %s" url)
        (kindel--download-text-content url msg-id
          (lambda (success)
            (unless success (setq any-failed t))
            (setq remaining (1- remaining))
            (when (= remaining 0)
              (if any-failed
                  (message "Kindel: Processing failed for message %s - tags not updated" msg-id)
                (kindel--update-message-tags msg-id)))))))))

(defun kindel--process-single-message (msg-id)
  "Process a single message MSG-ID."
  (message "Kindel processing message: %s" msg-id)
  (let ((raw-message (kindel--get-message-content msg-id)))
    (when raw-message
      (let ((urls (kindel-decode-and-extract-urls raw-message)))
        (message "Kindel extracted %d URLs from %s" (length urls) msg-id)
        (kindel--process-message-urls msg-id urls)))))

;;;###autoload
(defun kindel-process-new-emails ()
  "Process newly tagged emails automatically."
  (interactive)
  (let ((message-ids (kindel-get-tagged-emails kindel-processing-tag)))
    (message "Kindel found %d emails with tag '%s'" (length message-ids) kindel-processing-tag)
    (if message-ids
        (progn
          (dolist (msg-id message-ids)
            (kindel--process-single-message msg-id))
          (message "Kindel processing complete"))
      (message "Kindel: No emails found with tag '%s'. Use 'notmuch tag +%s <search>' to tag emails first." 
               kindel-processing-tag kindel-processing-tag))))

(provide 'kindel)

;;; notmuch-kindel.el ends here
