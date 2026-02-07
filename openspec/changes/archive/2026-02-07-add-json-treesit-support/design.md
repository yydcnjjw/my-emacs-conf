## Context

The current configuration uses `json-mode` for JSON editing. With Emacs 30+, `json-ts-mode` offers superior performance and accuracy via Tree-sitter. We have established patterns in `config/init-prog-language.el` for registering and configuring Tree-sitter modes (e.g., for YAML, Go).

## Goals / Non-Goals

**Goals:**
- Enable `json-ts-mode` for all `.json` files.
- Ensure the JSON tree-sitter grammar is registered and installable.
- Maintain consistency with existing tree-sitter configurations.

**Non-Goals:**
- Heavy customization of `json-ts-mode` (default settings are sufficient for now).
- adding LSP support for JSON (out of scope for this change, though `json-ts-mode` supports it).

## Decisions

### Configuration Location
**Decision:** Update `config/init-prog-language.el`.
**Rationale:** This file already contains configuration for general programming languages and their tree-sitter setups (Go, YAML, C++). Creating a separate `init-json.el` would be overkill for such a small configuration.

### Implementation Pattern
**Decision:** Follow the existing `use-package emacs :after treesit` pattern.
**Rationale:** This ensures tree-sitter is loaded before we try to register grammars. We will use `my/treesit-register` to define the grammar source.

### Mode Association
**Decision:** Explicitly map `.json` files to `json-ts-mode` via `:mode` or `auto-mode-alist`.
**Rationale:** This guarantees the new mode is used. We will also keep `json-mode` available but deprioritized or unused for `.json` files, effectively replacing it.

## Risks / Trade-offs

- **Risk:** Grammar compilation failure.
  - **Mitigation:** The `my/treesit-setup` advice logic handles checking availability, but initial installation depends on system tools (C compiler). This is an existing constraint for all tree-sitter modes in this config.
