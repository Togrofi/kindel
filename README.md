# Kindel

Extract URLs from tagged notmuch emails and download text content for processing.

## Installation

1. Download `notmuch-kindel.el` to your Emacs configuration directory
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

## Configuration

Available customization options:

```elisp
(setq kindel-processing-tag "kindel-to-process")  ; Tag for emails to process
(setq kindel-download-timeout 10)                ; Download timeout in seconds
(setq kindel-link-text-pattern "View")           ; Pattern to match in link text
```

## Functions

- `kindel-process-new-emails`: Process emails tagged with `kindel-processing-tag`
- `kindel-decode-and-extract-urls`: Decode quoted-printable message and extract URLs  
- `kindel-download-text-content`: Download text content from URL asynchronously

## Requirements

- Emacs 27.1 or later
- notmuch 0.32 or later

## License

GPL-3.0
