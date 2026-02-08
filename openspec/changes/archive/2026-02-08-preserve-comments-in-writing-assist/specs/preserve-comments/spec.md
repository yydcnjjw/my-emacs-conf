# Spec: Comment Preservation

## Requirement
The writing assistant MUST NOT remove or modify programming comment markers when improving text within a comment block.

## Details
1. When a sentence is selected for improvement, the system MUST identify any comment markers (e.g., `;;`, `//`, `#`) that are part of the line structure but not part of the natural language content.
2. The system SHOULD strip these markers from the text sent to the LLM to provide a cleaner context for improvement.
3. The replacement of the original text with the improved version MUST be targeted to the non-marker content.
4. If a sentence spans multiple lines, the markers on subsequent lines MUST also be preserved.

## Verification
- Test in `emacs-lisp-mode`: improving a sentence in a `;;` comment should keep the `;;`.
- Test in `c-mode`: improving a comment should keep the `//` or `/* */`.
- Test in `python-mode`: improving a comment should keep the `#`.
- Test with inline comments: `(setq x 1) ;; comment` -> improving `(setq x 1)` should keep `;; comment`.
