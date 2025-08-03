# Kindel

Extract URLs from tagged notmuch emails and download text content for processing.

## Installation

1. Download `kindel.el` to your Emacs configuration directory
2. Add to your init file:

```elisp
(add-to-list 'load-path "/path/to/kindel")
(require 'kindel)
```

## Usage

### Basic Setup

```elisp
;; Define your text processing function
(defun my-text-processor (text url message-id)
  "Process downloaded text content."
  (message "Processing %d chars from %s" (length text) url)
  ;; Your processing logic here
  )

;; Add to the list of processors
(add-to-list 'kindel-text-processors #'my-text-processor)

;; Process tagged emails
(kindel-process-new-emails)
```

### Automatic File Storage

Kindel automatically saves downloaded content to `~/kindel-downloads/` with filenames like:
`MESSAGE-ID-URL-HASH-TIMESTAMP.txt`

The built-in `kindel-save-to-file` processor is included by default.

## Configuration

Available customization options:

```elisp
(setq kindel-processing-tag "kindel")           ; Tag for emails to process (default)
(setq kindel-processed-tag "kindel-processed") ; Tag added after processing (default)
(setq kindel-download-timeout 10)              ; Download timeout in seconds
(setq kindel-link-text-pattern "View")         ; Pattern to match in link text
(setq kindel-allowed-senders '("do-not-reply@amazon.com")) ; Allowed email senders (default)
```

### Text Processors

Customize how downloaded content is processed:

```elisp
;; Replace default processors
(setq kindel-text-processors (list #'my-custom-processor))

;; Add additional processors
(add-to-list 'kindel-text-processors #'my-additional-processor)
```

## Functions

- `kindel-process-new-emails`: Process emails tagged with `kindel-processing-tag`

## Email Filtering and Processing

Kindel only processes emails from allowed senders for security:

```elisp
;; Allow multiple senders
(setq kindel-allowed-senders '("do-not-reply@amazon.com" 
                               "notifications@github.com"
                               "alerts@example.com"))
```

### Tag Management

Kindel automatically manages email tags during processing:
1. Finds emails tagged with `kindel-processing-tag` (default: "kindel")
2. Checks if sender is in `kindel-allowed-senders` list
3. Downloads content from matching URLs (if sender allowed)
4. Runs all registered text processors
5. On success: removes processing tag and adds `kindel-processed-tag` (default: "kindel-processed")

## Requirements

- Emacs 27.1 or later
- notmuch 0.32 or later

## License

GPL-3.0
