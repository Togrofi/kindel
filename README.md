# Kindel

Email tagging and HTML body extraction for notmuch in Emacs.

## Installation

### Manual Installation

1. Download `kindel.el` to your Emacs configuration directory
2. Add to your init file:

```elisp
(add-to-list 'load-path "/path/to/kindel")
(require 'kindel)
```

### Package Installation

Install from a package archive (when available):

```
M-x package-install RET kindel RET
```

## Usage

### Basic Usage

```elisp
;; Tag emails from a sender for processing
(kindel-tag-emails-for-processing "sender@example.com" "my-tag")

;; Get HTML bodies from tagged emails
(let ((html-bodies (kindel-process-tagged-emails "my-tag")))
  (dolist (html html-bodies)
    (your-processing-function html)))

;; Show tagged emails in notmuch
(kindel-show-tagged-emails "my-tag")
```

### Automation

Set up automatic processing with hooks:

```elisp
;; Add your processing function to the hook
(add-hook 'kindel-html-body-hook #'your-processing-function)

;; Enable automatic processing after notmuch tagging (optional)
(kindel-setup-auto-processing)

;; Or process emails manually
(kindel-process-new-emails)
```

## Configuration

Customize tags, link patterns, and auto-download settings:

```elisp
(setq kindel-default-tag "my-default-tag")
(setq kindel-processing-tag "my-processing-tag")
(setq kindel-link-text-pattern "View Document") ; Default is "View"

;; Auto-download configuration
(setq kindel-auto-download-enabled t) ; Enable auto-download (default: t)
(setq kindel-download-timeout 15) ; Download timeout in seconds (default: 10)

;; URL patterns for auto-download
(setq kindel-download-url-patterns
      '("\\bhttps?://[^[:space:]]+\\.txt\\b"
        "\\bhttps?://[^[:space:]]+/download\\b"
        "\\bhttps?://[^[:space:]]+\\.log\\b"))
```

### Link Extraction

Extract links from HTML email content where the visible text matches a pattern:

```elisp
;; Extract links with default pattern from HTML content
(let ((links (kindel-extract-links-with-text html-content)))
  (dolist (link links)
    (message "Found link: %s -> %s" 
             (plist-get link :text) 
             (plist-get link :url))))

;; Extract links with custom pattern
(let ((links (kindel-extract-links-with-text html-content "Download")))
  ;; Process links...
  )

;; Extract links from all emails with a specific tag
(let ((links (kindel-extract-links-from-tagged-emails "kindle-process" "View")))
  (dolist (link links)
    (message "Link from %s: %s" 
             (plist-get link :message-id)
             (plist-get link :url))))

;; Interactive function to view extracted links
M-x kindel-show-links-from-tagged-emails
```

### Auto-Download and Text Processing

Set up automatic downloading and processing of text files from email links:

```elisp
;; Register a text processing function
(defun my-text-processor (text url message-id)
  "Process downloaded text content."
  (message "Processing %d chars from %s (email: %s)" 
           (length text) url message-id)
  ;; Your text processing logic here
  (external-package-process-text text))

;; Method 1: Register via function call
(kindel-register-text-processor #'my-text-processor)

;; Method 2: Add to hook
(add-hook 'kindel-downloaded-text-hook #'my-text-processor)

;; Method 3: Add to custom variable
(add-to-list 'kindel-text-processors #'my-text-processor)

;; Enable auto-processing
(kindel-setup-auto-processing)
```

## Advanced Configuration Examples

### Using use-package (Recommended)

```elisp
(use-package kindel
  :commands (kindel-process-new-emails 
             kindel-show-tagged-emails
             kindel-setup-auto-processing)
  :config
  ;; Set up your HTML processing function
  (defun my-html-processor (html-bodies)
    "Process HTML bodies from emails."
    (dolist (html html-bodies)
      ;; Your processing logic here
      (message "Processing HTML: %s..." (substring html 0 50))))
  
  ;; Add your function to the hook
  (add-hook 'kindel-html-body-hook #'my-html-processor)
  
  ;; Optional: Enable automatic processing
  (kindel-setup-auto-processing)
  
  ;; Optional: Customize tags, patterns, and auto-download
  (setq kindel-default-tag "my-emails")
  (setq kindel-processing-tag "process-me")
  (setq kindel-link-text-pattern "View Document")
  (setq kindel-auto-download-enabled t)
  (setq kindel-download-timeout 15))
```

### With Multiple Processing Functions

```elisp
(use-package kindel
  :commands (kindel-process-new-emails)
  :config
  ;; Multiple processing functions
  (defun extract-todos (html-bodies)
    "Extract todos from HTML bodies."
    (dolist (html html-bodies)
      ;; Extract and save todos
      (when (string-match-p "TODO\\|FIXME\\|NOTE" html)
        (message "Found todo in email"))))
  
  (defun generate-summaries (html-bodies)
    "Generate summaries from HTML bodies."
    (dolist (html html-bodies)
      ;; Generate and save summaries
      (message "Generated summary for email")))
  
  ;; Add multiple processors
  (add-hook 'kindel-html-body-hook #'extract-todos)
  (add-hook 'kindel-html-body-hook #'generate-summaries)
  
  ;; Auto-process emails
  (kindel-setup-auto-processing))
```

### Auto-Download Text Processing

```elisp
(use-package kindel
  :custom
  (kindel-auto-download-enabled t)
  (kindel-download-timeout 20)
  (kindel-download-url-patterns 
   '("\\bhttps?://[^[:space:]]+\\.txt\\b"
     "\\bhttps?://[^[:space:]]+/download\\b"
     "\\bhttps?://kindle-scribe\\.amazon\\.com/[^[:space:]]+\\b"))
  :config
  ;; Text processing function
  (defun process-downloaded-text (text url message-id)
    "Process downloaded text from email links."
    (message "Downloaded %d chars from %s" (length text) url)
    ;; Send to external package for processing
    (when (fboundp 'external-text-analyzer)
      (external-text-analyzer text :source url :email-id message-id))
    ;; Or save to file for later processing
    (when (> (length text) 100)
      (let ((filename (format "/tmp/kindel-%s.txt" 
                             (format-time-string "%Y%m%d-%H%M%S"))))
        (with-temp-file filename
          (insert text))
        (message "Saved downloaded content to %s" filename))))
  
  ;; Register the processor
  (kindel-register-text-processor #'process-downloaded-text)
  
  ;; Alternative: use hook
  (add-hook 'kindel-downloaded-text-hook #'process-downloaded-text)
  
  ;; Enable auto-processing
  (kindel-setup-auto-processing))
```

### Integration with Specific Email Sources

```elisp
(use-package kindel
  :config
  ;; Tag Kindle emails automatically
  (defun setup-kindle-processing ()
    "Set up processing for Kindle emails."
    (kindel-tag-emails-for-processing "do-not-reply@kindle.com" "kindle-process")
    (message "Kindle email processing configured"))
  
  ;; Processing function for Kindle content
  (defun process-kindle-emails (html-bodies)
    "Process Kindle Scribe emails specifically."
    (dolist (html html-bodies)
      ;; Extract highlights, notes, etc.
      (when (string-match-p "kindle\\|scribe" html)
        (message "Processing Kindle content"))))
  
  ;; Extract links from Kindle emails
  (defun extract-kindle-links ()
    "Extract links from Kindle emails with 'View' text."
    (let ((links (kindel-extract-links-from-tagged-emails "kindle-process" "View")))
      (dolist (link links)
        (message "Kindle link: %s" (plist-get link :url))
        ;; Open or process the link as needed
        (browse-url (plist-get link :url)))))
  
  (add-hook 'kindel-html-body-hook #'process-kindle-emails)
  (kindel-setup-auto-processing)
  
  ;; Run setup
  (setup-kindle-processing))
```

### Complete Example with Text Processing

```elisp
(use-package kindel
  :ensure nil  ; if installing manually
  :commands (kindel-process-new-emails 
             kindel-show-tagged-emails
             kindel-tag-emails-for-processing)
  :bind (("C-c k p" . kindel-process-new-emails)
         ("C-c k s" . kindel-show-tagged-emails)
         ("C-c k l" . kindel-show-links-from-tagged-emails))
  :custom
  (kindel-default-tag "important")
  (kindel-processing-tag "ai-process")
  (kindel-auto-download-enabled t)
  (kindel-download-timeout 15)
  :config
  ;; Main processing function
  (defun my-email-ai-processor (html-bodies)
    "Send HTML email bodies to AI for processing."
    (dolist (html html-bodies)
      ;; Your AI processing logic here
      ;; Could integrate with GPT, Claude, local models, etc.
      (message "Sending to AI: %d chars" (length html))))
  
  ;; Text processing for downloaded content
  (defun my-text-downloader-processor (text url message-id)
    "Process downloaded text content with AI."
    (message "Processing downloaded text: %d chars from %s" (length text) url)
    ;; Send text to AI processing
    (when (fboundp 'ai-process-text)
      (ai-process-text text :source url :context "email-download")))
  
  ;; Set up the hooks
  (add-hook 'kindel-html-body-hook #'my-email-ai-processor)
  (kindel-register-text-processor #'my-text-downloader-processor)
  
  ;; Enable auto-processing
  (kindel-setup-auto-processing)
  
  ;; Tag specific senders
  (kindel-tag-emails-for-processing "newsletter@example.com" "ai-process")
  (kindel-tag-emails-for-processing "reports@work.com" "ai-process"))
```

## Functions

- `kindel-process-tagged-emails`: Get HTML bodies from tagged emails
- `kindel-tag-emails-for-processing`: Tag emails from sender
- `kindel-show-tagged-emails`: Show tagged emails in notmuch
- `kindel-process-new-emails`: Process emails and run hooks
- `kindel-count-tagged-emails`: Count emails with tag
- `kindel-remove-processing-tag`: Remove processing tag from emails
- `kindel-extract-links-with-text`: Extract links from HTML with matching visible text
- `kindel-extract-links-from-tagged-emails`: Extract links from all tagged emails
- `kindel-show-links-from-tagged-emails`: Interactive function to display extracted links
- `kindel-register-text-processor`: Register function to process downloaded text
- `kindel-unregister-text-processor`: Unregister text processing function
- `kindel-extract-download-urls`: Extract auto-download URLs from HTML
- `kindel-download-text-content`: Download text content from URL asynchronously

## Requirements

- Emacs 27.1 or later
- notmuch 0.32 or later

## License

GPL-3.0