;;; test-kindel.el --- Tests for kindel.el -*- lexical-binding: t; -*-

;; Load the kindel package
(load-file "notmuch-kindel.el")

;; Test data with quoted-printable encoding and href attributes
(defvar test-raw-message
  "Content-Type: text/html; charset=utf-8
Content-Transfer-Encoding: quoted-printable

<html>
<body>
<p>Check out this <a href=3D\"https://example.com/file.txt\">text file</a></p>
<p>And this <a href=3D\"https://amazon.com/product/123\">Amazon link</a></p>
<p>Also see <a href=3D\"https://site.com/download\">download page</a></p>
</body>
</html>")

;; Test data with multiline URLs (soft line breaks)
(defvar test-multiline-message
  "Content-Type: text/html; charset=utf-8
Content-Transfer-Encoding: quoted-printable

<html>
<body>
<p>Long URL: <a href=3D\"https://very-long-domain-name.example.com/path/=
to/some/very/long/file.txt?param1=3Dvalue1&param2=3Dvalue2\">link</a></p>
<p>Another: <a href=3D\"https://amazon.com/product/very-long-product-nam=
e-here/dp/B123456789\">product</a></p>
</body>
</html>")

;; Test the updated kindel-decode-and-extract-urls function
(defun test-kindel-decode-and-extract-urls ()
  "Test the updated kindel-decode-and-extract-urls function."
  (let ((urls (kindel-decode-and-extract-urls test-raw-message)))
    (message "Extracted URLs: %s" urls)
    urls))

;; Test with regex filtering
(defun test-kindel-decode-and-extract-urls-with-regex ()
  "Test the updated kindel-decode-and-extract-urls function with regex filtering."
  (let* ((all-urls (kindel-decode-and-extract-urls test-raw-message))
         (urls (seq-filter (lambda (url) (string-match "amazon\\.com" url)) all-urls)))
    (message "Amazon URLs: %s" urls)
    urls))

;; Test multiline URLs
(defun test-multiline-urls ()
  "Test extraction of URLs that span multiple lines."
  (let ((urls (kindel-decode-and-extract-urls test-multiline-message)))
    (message "Multiline URLs: %s" urls)
    urls))

;; Test with real email data
(defun test-real-email-data ()
  "Test extraction of URLs from the real email message file."
  (let* ((raw-email (with-temp-buffer
                      (insert-file-contents "test-raw-message")
                      (buffer-string)))
         (urls (kindel-decode-and-extract-urls raw-email)))
    (message "Real email URLs: %s" urls)
    urls))

;; Test processor function
(defun kindel-test-processor (text url message-id)
  "Test processor that writes downloaded text to a file.
Arguments: TEXT is the downloaded content, URL is the source URL, MESSAGE-ID is the email ID."
  (let ((filename (format "kindel-test-%s.txt" (substring message-id 0 8))))
    (with-temp-file filename
      (when (file-exists-p filename)
        (insert-file-contents filename))
      (goto-char (point-max))
      (insert text)
      (insert "\n\n"))
    (message "Kindel test: Written %d chars from %s to file %s" 
             (length text) url filename)))

(defun kindel-setup-test ()
  "Setup kindel for testing with the test processor."
  (interactive)
  (add-hook 'kindel-downloaded-text-hook #'kindel-test-processor)
  (message "Kindel test processor added to hook"))

(defun kindel-test-message-id (message-id)
  "Test kindel processing for a specific MESSAGE-ID."
  (interactive "sMessage ID: ")
  (let ((raw-message (kindel-get-message-content message-id)))
    (if raw-message
        (let ((urls (kindel-decode-and-extract-urls raw-message)))
          (message "Found %d URLs in message %s: %s" 
                   (length urls) message-id urls)
          (dolist (url urls)
            (kindel-download-text-content url message-id))
          urls)
      (message "Could not retrieve message: %s" message-id))))

;; Run tests
(message "Testing kindel-decode-and-extract-urls...")
(test-kindel-decode-and-extract-urls)
(test-kindel-decode-and-extract-urls-with-regex)
(message "Testing multiline URLs...")
(test-multiline-urls)
(message "Testing real email data...")
(test-real-email-data)
(message "Tests completed.")