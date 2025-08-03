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

;;; Configuration

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

;;; Processing State Tracking

(defvar kindel--processing-state (make-hash-table :test 'equal)
  "Hash table tracking processing state for each message.
Key: message-id, Value: (total-urls completed-urls failed-urls).")

(defun kindel-clear-processing-state ()
  "Clear all processing state. Useful for cleanup or testing."
  (clrhash kindel--processing-state))

(defun kindel--init-message-processing (message-id url-count)
  "Initialize processing state for MESSAGE-ID with URL-COUNT URLs."
  (puthash message-id (list url-count 0 0) kindel--processing-state))

(defun kindel--calculate-new-state (current-state success)
  "Calculate new processing state from CURRENT-STATE and SUCCESS status.
Returns (new-state . completion-status) where completion-status is
:success, :failed, or nil if still processing."
  (let* ((total (car current-state))
         (completed (1+ (cadr current-state)))
         (failed (if success (caddr current-state) (1+ (caddr current-state))))
         (new-state (list total completed failed))
         (completion-status (when (>= completed total)
                             (if (> failed 0) :failed :success))))
    (cons new-state completion-status)))

(defun kindel--mark-url-completed (message-id success)
  "Mark one URL as completed for MESSAGE-ID with SUCCESS status.
Returns :success if all URLs succeeded, :failed if any failed, nil if still processing."
  (let ((current-state (gethash message-id kindel--processing-state)))
    (when current-state
      (let* ((state-result (kindel--calculate-new-state current-state success))
             (new-state (car state-result))
             (completion-status (cdr state-result)))
        (if completion-status
            (remhash message-id kindel--processing-state)
          (puthash message-id new-state kindel--processing-state))
        completion-status))))

;;; Error Handling Utilities

(defun kindel--with-error-handling (operation description &rest args)
  "Execute OPERATION with consistent error handling and logging.
DESCRIPTION should describe what operation is being performed.
ARGS are passed to the operation function."
  (condition-case err
      (apply operation args)
    (error
     (message "Kindel error %s: %s" description (error-message-string err))
     nil)))

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
  (kindel--with-error-handling
   (lambda ()
     (notmuch-call-notmuch-sexp
      "search"
      "--format=sexp"
      "--output=messages"
      (format "tag:%s" tag)))
   "getting tagged emails"))

(defun kindel--get-message-content (message-id)
  "Get the complete raw message content from MESSAGE-ID."
  (kindel--with-error-handling
   (lambda ()
     (notmuch-command-to-string "show" "--format=raw" (format "id:%s" message-id)))
   "getting message content"))

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
(defun kindel--make-download-callback (url message-id)
  "Create download response callback for URL and MESSAGE-ID."
  (lambda (status)
    (kindel--handle-download-response status url message-id)))

(defun kindel--handle-network-error (url message-id err)
  "Handle network/connection errors when starting download for URL and MESSAGE-ID."
  (message "Kindel network error for %s: %s" url (error-message-string err))
  (let ((completion-result (kindel--mark-url-completed message-id nil)))
    (when (eq completion-result :failed)
      (message "Kindel: Processing failed for message %s - tags not updated" message-id))))

(defun kindel--download-text-content (url message-id)
  "Download text content from URL asynchronously with redirect following and process it."
  (condition-case err
      (url-retrieve url
                    (kindel--make-download-callback url message-id)
                    nil t kindel-download-timeout)
    (error
     (kindel--handle-network-error url message-id err))))

(defun kindel--extract-response-content ()
  "Extract content from HTTP response buffer after headers."
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (when (re-search-forward "^$" nil t)
    (buffer-substring-no-properties (point) (point-max))))

(defun kindel--handle-http-error (status url message-id)
  "Handle HTTP response errors and mark URL as failed."
  (message "Kindel HTTP error for %s: %s" url (plist-get status :error))
  (kindel--mark-url-completed message-id nil))

(defun kindel--handle-download-success (url message-id)
  "Handle successful download and process content."
  (let ((content (kindel--extract-response-content)))
    (if (and content (not (string-empty-p content)))
        (progn
          (message "Kindel downloaded %d chars from %s" (length content) url)
          (kindel--process-downloaded-text content url message-id))
      (kindel--mark-url-completed message-id nil))))

(defun kindel--handle-download-response (status url message-id)
  "Handle HTTP response from download and stream content to processors."
  (condition-case err
      (if (plist-get status :error)
          (kindel--handle-http-error status url message-id)
        (kindel--handle-download-success url message-id))
    (error
     (message "Kindel response handling error for %s: %s" url (error-message-string err))
     (kindel--mark-url-completed message-id nil)))
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

(defun kindel--handle-completion-result (completion-result message-id)
  "Handle the completion result and update tags if appropriate."
  (cond
   ((eq completion-result :success)
    (kindel--update-message-tags message-id))
   ((eq completion-result :failed)
    (message "Kindel: Processing failed for message %s - tags not updated" message-id))))

(defun kindel--process-downloaded-text (text url message-id)
  "Process downloaded text content through all registered processors."
  (let* ((processing-success (kindel--run-processors text url message-id))
         (completion-result (kindel--mark-url-completed message-id processing-success)))
    (kindel--handle-completion-result completion-result message-id)))

;;; Interactive Functions

(defun kindel--process-message-urls (msg-id urls)
  "Process URLs for a single message MSG-ID.
If no URLs are found, immediately update message tags.
Otherwise, initialize processing state and download each URL."
  (if urls
      (progn
        (kindel--init-message-processing msg-id (length urls))
        (dolist (url urls)
          (message "Kindel downloading from: %s" url)
          (kindel--download-text-content url msg-id)))
    (message "Kindel: No URLs found in message %s" msg-id)
    (kindel--update-message-tags msg-id)))

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
