## Why

Enhance JSON editing capabilities by enabling Tree-sitter support (`json-ts-mode`) in Emacs 30+. This provides more accurate syntax highlighting, better structural navigation, and improved performance compared to the legacy regex-based `json-mode`.

## What Changes

- Enable `json-ts-mode` for JSON files.
- Register the JSON tree-sitter grammar using the project's `my/treesit-register` facility.
- Remap or replace usage of `json-mode` with `json-ts-mode`.

## Capabilities

### New Capabilities
- `json-treesit`: Provides Tree-sitter based syntax highlighting and editing for JSON files.

### Modified Capabilities
- `json-mode`: Will be superseded by `json-ts-mode` for JSON editing.

## Impact

- **Configuration**: `config/init-prog-language.el` will be updated to include JSON tree-sitter configuration.
- **Dependencies**: Requires `json` tree-sitter grammar (handled by `treesit-install-language-grammar` via `my/treesit-register`).
