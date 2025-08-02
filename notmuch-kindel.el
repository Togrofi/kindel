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
;;   ;; Use automation hooks
;;   (add-hook 'kindel-raw-message-hook #'your-processing-function)
;;   ;; Or call manually:
;;   (kindel-process-new-emails)
;;
;; Main functions:
;; - `kindel-process-new-emails': Process emails and run hooks

;;; Code:

(require 'notmuch)
(require 'json)
(require 'qp)
(require 'seq)

;;; Configuration

(defcustom kindel-processing-tag "kindel-to-process"
  "Tag applied to emails that need processing."
  :type 'string
  :group 'kindel)

(defcustom kindel-downloaded-text-hook nil
  "Hook run with downloaded text content from email links.
Functions in this hook receive (text url message-id) as arguments."
  :type 'hook
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

(defun kindel-get-raw-message (message-id)
  "Extract HTML body content from MESSAGE-ID."
  (condition-case err
      (let ((message-data (notmuch-command-to-string "show" "--format=raw" (format "id:%s" message-id))))
        message-data)
    (error
     (message "Kindel error getting message body: %s" (error-message-string err))
     nil)))

(defun kindel-extract-urls (raw-message)
  "Return a list of URLs extracted from href attributes in RAW-MESSAGE.
First handles quoted-printable soft line breaks, then decodes and extracts URLs."
  (when raw-message
    (let* (;; First remove soft line breaks (= at end of line followed by newline)
	   (no-soft-breaks (replace-regexp-in-string "=\n" "" raw-message))
	   ;; Then decode quoted-printable
	   (decoded-message (quoted-printable-decode-string no-soft-breaks))
	   (href-regex "href=\"\\([^\"]+\\)\"")
	   (start 0)
	   urls)
      ;; Extract all URLs from href attributes
      (while (string-match href-regex decoded-message start)
	(let ((url (match-string 1 decoded-message)))
	  (when (and url (not (string-empty-p url)))
	    (push url urls)))
	(setq start (match-end 0)))
      ;; Filter by regex if provided
      urls)))

;;; HTTP Download Functions
(defun kindel-download-text-content (url message-id)
  "Download text content from URL asynchronously with redirect following and process it."
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

;;; Interactive Functions

;;;###autoload
(defun kindel-process-new-emails ()
  "Process newly tagged emails automatically."
  (interactive)
  (let ((message-ids (kindel-get-tagged-emails kindel-processing-tag)))
    (dolist (msg-id message-ids)
      (let ((raw-message (kindel-get-raw-message msg-id)))
	(when raw-message
	  (let ((urls (kindel-extract-urls raw-message)))
	    (dolist (url urls)
	      (kindel-download-text-content url msg-id)))))))))

(provide 'kindel)

;;; kindel.el ends here
