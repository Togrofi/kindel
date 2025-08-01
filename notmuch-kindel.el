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

;;; Automation Hooks

;;;###autoload
(defun kindel-process-new-emails ()
  "Process newly tagged emails automatically."
  (interactive)
  (let ((html-bodies (kindel-process-tagged-emails kindel-processing-tag)))
    (when html-bodies
      (message "Kindel: Found %d emails with HTML content" (length html-bodies))
      (run-hook-with-args 'kindel-html-body-hook html-bodies))))

(defcustom kindel-html-body-hook nil
  "Hook run with HTML bodies from processed emails.
Functions in this hook receive a list of HTML body strings."
  :type 'hook
  :group 'kindel)

;;; Hook Integration

(defun kindel-setup-auto-processing ()
  "Set up automatic processing after notmuch tagging operations.
Add this to your init.el if you want automatic processing:
  (with-eval-after-load 'kindel (kindel-setup-auto-processing))"
  (interactive)
  (add-hook 'notmuch-after-tag-hook #'kindel-process-new-emails)
  (message "Kindel auto-processing enabled"))

;;; Utility Functions

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
