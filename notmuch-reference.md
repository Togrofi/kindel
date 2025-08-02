# Notmuch Email Query Reference

This document contains key information about using notmuch for email querying, particularly from Emacs Lisp.

## Core Concepts

Notmuch is a fast, global-search and tag-based email system. Key principles:
- **Search-based**: All operations are fundamentally search-based
- **Tag-based**: Emails are organized using tags rather than folders
- **Full-text search**: Complete content indexing and search capabilities

## Tag System

### Default Tags
- `inbox`: New messages automatically tagged
- `unread`: Unread messages
- `sent`: Sent messages

### Custom Tags
- Tags are simple words like "kindle-scribe", "work", "lists", etc.
- Tags can contain hyphens and underscores
- Use quotes for tags with spaces: `tag:"a tag"`

## Command Line Interface

### Basic Search Syntax
```bash
notmuch search [options] <search-terms>
```

### Format Options
- `--format=(json|sexp|text|text0)`
  - `json`: JSON output
  - `sexp`: S-expression output (best for Emacs Lisp)
  - `text`: Plain text (default)
  - `text0`: Null-separated text

### Output Options
- `--output=(summary|threads|messages|files|tags)`
  - `summary`: Thread summaries with metadata
  - `threads`: Thread IDs only
  - `messages`: Message IDs only
  - `files`: File paths to email messages
  - `tags`: All tags on matching messages

### Search Terms
- `tag:tagname` or `is:tagname`: Search by tag
- `date:today..` or `date:2024-01-01..2024-12-31`: Date ranges
- `from:email@domain.com`: From address
- `to:email@domain.com`: To address
- `subject:keyword`: Subject line
- `body:keyword`: Message body
- `*`: Match all messages

### Examples
```bash
# Find all kindle-scribe emails
notmuch search tag:kindle-scribe

# Get message IDs in S-expression format
notmuch search --format=sexp --output=messages tag:kindle-scribe

# Get file paths for kindle-scribe emails
notmuch search --format=sexp --output=files tag:kindle-scribe

# Complex query
notmuch search "tag:kindle-scribe AND date:2024.."
```

## Emacs Lisp Interface

### Core Functions

#### `notmuch-call-notmuch-sexp`
Primary function for programmatic queries:
```elisp
(notmuch-call-notmuch-sexp &rest args)
```
- Returns parsed S-expression results
- Signals error on non-zero exit status
- Supports `:stdin-string` keyword argument

#### `notmuch-call-notmuch-process`
Synchronous command execution:
```elisp
(notmuch-call-notmuch-process &rest args)
```
- Returns exit status
- Used for operations that don't need output parsing

#### `notmuch-start-notmuch`
Asynchronous process execution:
```elisp
(notmuch-start-notmuch name buffer sentinel &rest args)
```
- Non-blocking execution
- Requires process sentinel for completion handling

### Interactive Functions

#### `notmuch-search`
Open search results buffer:
```elisp
(notmuch-search query &optional oldest-first target-thread target-line)
```

#### `notmuch`
Main notmuch interface with saved searches

### Practical Examples

#### Get all emails with specific tag
```elisp
(defun get-tagged-emails (tag)
  "Return all emails with specified TAG."
  (notmuch-call-notmuch-sexp
   "search"
   "--format=sexp"
   "--output=messages"
   (format "tag:%s" tag)))
```

#### Get thread summaries
```elisp
(defun get-tag-summaries (tag)
  "Return thread summaries for emails with TAG."
  (notmuch-call-notmuch-sexp
   "search"
   "--format=sexp"
   "--output=summary"
   (format "tag:%s" tag)))
```

#### Get file paths
```elisp
(defun get-tag-files (tag)
  "Return file paths for emails with TAG."
  (notmuch-call-notmuch-sexp
   "search"
   "--format=sexp"
   "--output=files"
   (format "tag:%s" tag)))
```

## Configuration

### Variables
- `notmuch-command`: Path to notmuch binary (customizable)
- `notmuch-hello-hide-tags`: Tags to hide in hello view
- `notmuch-search-oldest-first`: Default sort order

### Saved Searches
Configure commonly used searches:
```elisp
(setq notmuch-saved-searches
      '((:name "inbox" :query "tag:inbox")
        (:name "unread" :query "tag:unread")
        (:name "kindle-scribe" :query "tag:kindle-scribe")))
```

## Error Handling

Always wrap notmuch calls in `condition-case`:
```elisp
(condition-case err
    (notmuch-call-notmuch-sexp "search" "tag:nonexistent")
  (error
   (message "Notmuch error: %s" (error-message-string err))))
```

## Performance Considerations

- S-expression format is most efficient for Emacs Lisp
- Use specific output types (`--output=messages`) to reduce data transfer
- Consider using `--limit` for large result sets
- Asynchronous calls for UI responsiveness

## Use Cases for Our Project

1. **Email Discovery**: Find emails by tag for processing
2. **Content Extraction**: Get file paths for email content analysis
3. **Batch Processing**: Process sets of tagged emails
4. **Integration**: Combine with other Emacs tools for workflow automation

## Automated Email Processing Workflow

### Auto-tagging Setup
Configure notmuch to automatically tag emails on arrival:

```bash
# In .notmuch-config
[hooks]
post_new=notmuch tag +kindle-scribe -- from:do-not-reply@kindle.com
```

### Email Body Extraction
Extract plain text content from emails:

```elisp
(defun get-message-body (message-id)
  "Extract plain text body from notmuch message."
  (let* ((message-data (notmuch-call-notmuch-sexp
                       "show" "--format=sexp" message-id))
         (message-part (car message-data))
         (body-part (cdr (assq 'body message-part))))
    (extract-text-from-part body-part)))
```

### Integration with Content Processing
Process email bodies through text analysis tools:

```elisp
(defun process-tagged-emails (tag)
  "Process all emails with TAG through wrangel."
  (let ((message-ids (notmuch-call-notmuch-sexp
                     "search" "--format=sexp" "--output=messages"
                     (format "tag:%s" tag))))
    (dolist (msg-id message-ids)
      (let ((body (get-message-body msg-id)))
        (when body
          (wrangel-todo-from-text body)
          (wrangel-ideas-from-text body)
          (wrangel-tldr-from-text body))))))
```

### Automation Hooks
Set up automatic processing of new emails:

```elisp
;; Process new emails after tagging
(add-hook 'notmuch-after-tag-hook #'process-new-tagged-emails)

;; Daily batch processing
(run-at-time "09:00" 86400 #'process-recent-emails)
```