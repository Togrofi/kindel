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

;; Process emails automatically
(kindel-process-new-emails)
```

## Configuration

Customize the default tags:

```elisp
(setq kindel-default-tag "my-default-tag")
(setq kindel-processing-tag "my-processing-tag")
```

## Functions

- `kindel-process-tagged-emails`: Get HTML bodies from tagged emails
- `kindel-tag-emails-for-processing`: Tag emails from sender
- `kindel-show-tagged-emails`: Show tagged emails in notmuch
- `kindel-process-new-emails`: Process emails and run hooks
- `kindel-count-tagged-emails`: Count emails with tag
- `kindel-remove-processing-tag`: Remove processing tag from emails

## Requirements

- Emacs 27.1 or later
- notmuch 0.32 or later

## License

GPL-3.0