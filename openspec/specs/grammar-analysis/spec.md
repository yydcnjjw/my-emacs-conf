## ADDED Requirements

### Requirement: Mode-Aware Sentence Selection
The system SHALL automatically select the most relevant sentence or text block based on the current major mode and cursor position when grammar analysis is triggered.

#### Scenario: Selection in Org-mode
- **WHEN** user triggers grammar check in `org-mode` without an active region
- **THEN** system selects the current sentence using `thing-at-point 'sentence` logic

#### Scenario: Selection in Prog-mode
- **WHEN** user triggers grammar check in `prog-mode` inside a comment block
- **THEN** system extracts the comment content using `separedit` logic and selects the relevant sentence within it

#### Scenario: Selection with Active Region
- **WHEN** user triggers grammar check with text selected
- **THEN** system uses the selected text regardless of mode

### Requirement: Side-Window Grammar Interface
The system SHALL display grammar analysis results in a dedicated side-window buffer `*my/grammar*` positioned at the right side of the frame.

#### Scenario: Displaying Analysis Results
- **WHEN** grammar analysis is complete
- **THEN** the side window displays a list of errors/suggestions and a sentence structure breakdown
- **AND** the window does not steal focus from the user's editing buffer

### Requirement: Interactive Grammar Corrections
The system SHALL allow users to apply suggested grammar corrections directly from the analysis results.

#### Scenario: Applying a Fix
- **WHEN** user clicks or activates a "Apply Fix" button for a suggested correction
- **THEN** the incorrect text in the original buffer is replaced with the suggested correction
