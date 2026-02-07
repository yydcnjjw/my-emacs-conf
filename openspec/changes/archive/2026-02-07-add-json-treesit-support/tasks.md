## 1. Implementation

- [x] 1.1 Update `config/init-prog-language.el` to register JSON tree-sitter grammar using `my/treesit-register`.
- [x] 1.2 Configure `json-ts-mode` as the default mode for `.json` files (using `auto-mode-alist` or `major-mode-remap-alist`).
- [x] 1.3 Remove or update legacy `json-mode` configuration to avoid conflicts.

## 2. Verification

- [x] 2.1 Verify that opening a `.json` file triggers `json-ts-mode`.
- [x] 2.2 Verify that the JSON tree-sitter grammar can be installed and loaded successfully.
