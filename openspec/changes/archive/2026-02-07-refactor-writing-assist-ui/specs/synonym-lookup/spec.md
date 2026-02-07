## MODIFIED Requirements

### Requirement: Insert Synonym
The system SHALL provide a mechanism to insert a selected synonym into the editing buffer at the cursor position.

#### Scenario: Inserting a Word
- **WHEN** user presses `RET` or number key (e.g. `1`) on a synonym in the side window
- **THEN** the selected word is inserted at point in the active buffer

## ADDED Requirements

### Requirement: Keyboard Navigation
The system SHALL allow users to navigate through synonym list using keyboard shortcuts.

#### Scenario: Navigating Items
- **WHEN** user presses `n` or `p` in the `*my/synonym*` buffer
- **THEN** point moves to the next/previous synonym
