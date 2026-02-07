## MODIFIED Requirements

### Requirement: Interactive Grammar Corrections
The system SHALL allow users to apply suggested grammar corrections directly from the analysis results.

#### Scenario: Applying a Fix
- **WHEN** user presses `RET` or number key (e.g. `1`) on a suggested correction in the side window
- **THEN** the incorrect text in the original buffer is replaced with the suggested correction

## ADDED Requirements

### Requirement: Keyboard Navigation
The system SHALL allow users to navigate through grammar suggestions using keyboard shortcuts.

#### Scenario: Navigating Items
- **WHEN** user presses `n` or `p` in the `*my/grammar*` buffer
- **THEN** point moves to the next/previous actionable suggestion
