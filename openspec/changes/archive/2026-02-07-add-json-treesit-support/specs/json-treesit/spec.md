## ADDED Requirements

### Requirement: JSON Tree-sitter Support
The system SHALL use `json-ts-mode` for JSON file editing to leverage Tree-sitter capabilities.

#### Scenario: Opening a JSON file
- **WHEN** user opens a file with `.json` extension
- **THEN** the major mode is `json-ts-mode`
- **THEN** syntax highlighting corresponds to Tree-sitter font-lock rules

### Requirement: Grammar Registration
The system SHALL register the JSON Tree-sitter grammar source to enable installation and usage.

#### Scenario: Registering grammar
- **WHEN** Emacs initializes or `init-prog-language.el` is loaded
- **THEN** the `json` language source is registered with `my/treesit-register`
- **THEN** the source URL points to `https://github.com/tree-sitter/tree-sitter-json`
