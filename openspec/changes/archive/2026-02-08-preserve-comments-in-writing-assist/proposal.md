# Proposal: Preserve Comments in Writing Assist

## Problem
When using writing assistant tools (like sentence improvement or grammar check) in code buffers, the tool currently replaces the entire selected region (sentence) with the LLM-generated text. This often causes the loss of programming comment markers (e.g., `;; ` in Emacs Lisp) because:
1. The selection includes the markers.
2. The LLM returns text without markers (or with different ones).
3. The replacement logic (`delete-region` + `insert`) blindly overwrites the markers.

## Goals
- Ensure that comment markers and inline comments are preserved when improving text in code buffers.
- Improve the reliability of the replacement logic by targeting specific substrings instead of entire regions.

## Proposed Changes
1. **Strip Markers from Input**: Modify `my/select-sentence-at-point` to return the "content" of the sentence, stripped of comment markers, while still providing the original buffer bounds.
2. **Safe Replacement**: Update `my/render-improve-result` to use `search-forward` and `replace-match` based on the `original` text (stripped) instead of `delete-region` on the absolute bounds.
3. **Handle Fallbacks**: If the stripped original text cannot be found in the buffer (e.g., due to formatting changes), provide a safe fallback or warn the user.

## Impact
- Better user experience when editing docstrings, comments, or code with inline comments.
- More idiomatic integration with programming modes.
