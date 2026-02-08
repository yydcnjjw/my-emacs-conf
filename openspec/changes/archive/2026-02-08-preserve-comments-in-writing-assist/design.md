# Design: Preserve Comments in Writing Assist

## Architecture

The system consists of three main parts:
1. **Selection**: Identifying the text to be improved.
2. **LLM Interaction**: Sending text and receiving suggestions.
3. **Application**: Replacing the original text with the selected suggestion.

### 1. Selection Refinement (`my/select-sentence-at-point`)

Currently, this function returns `(buffer-substring-no-properties beg end)`.
We will change it to return a list: `(content beg end original)`.

- `content`: The text with comment markers stripped.
- `beg`/`end`: The original bounds in the buffer.
- `original`: The raw text with markers (for potential fallback).

**Stripping Logic**:
Use `separedit--block-info` to get the `:comment-delimiter` regex.
Apply this regex to the text to remove markers from each line.

### 2. Application Logic (`my/render-improve-result`)

The action lambda will be updated:

```elisp
(lambda (d)
  (with-current-buffer (plist-get d :source-buffer)
    (save-excursion
      (goto-char (plist-get d :start-pos))
      (let ((limit (plist-get d :end-pos))
            (original (plist-get d :original))
            (new-text (plist-get d :text)))
        (if (search-forward original limit t)
            (replace-match new-text)
          (message "Warning: Could not find exact text to replace."))))))
```

### 3. Consistency

By stripping markers *before* sending to the LLM, the LLM's `original` field in the JSON response will match the stripped content in the buffer. This makes the `search-forward` logic highly reliable and marker-agnostic.

## Edge Cases

| Scenario | Behavior |
|----------|----------|
| Multi-line sentence | Markers stripped from all lines; `search-forward` matches across lines. |
| No markers (Markdown) | Stripping is a no-op; behavior remains same. |
| Inline comments | `search-forward` stops before the comment start if `original` doesn't include it. |
| Search fails | Fallback to user notification; no destructive replacement. |
