## MODIFIED Requirements

### Requirement: Replace Original Sentence
The system SHALL allow users to replace the original sentence with a selected improved version.

#### Scenario: Replacing Text
- **WHEN** user presses `RET` or number key (e.g. `1`) on a suggestion in the side window
- **THEN** the original sentence in the buffer is replaced by the selected improved version

## ADDED Requirements

### Requirement: Keyboard Navigation
The system SHALL allow users to navigate through improvement suggestions using keyboard shortcuts.

#### Scenario: Navigating Items
- **WHEN** user presses `n` or `p` in the `*my/improve*` buffer
- **THEN** point moves to the next/previous suggestion
