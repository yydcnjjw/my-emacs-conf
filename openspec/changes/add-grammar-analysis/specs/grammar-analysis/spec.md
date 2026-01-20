## ADDED Requirements

### Requirement: Sentence Selection
The system SHALL automatically select the current sentence around the cursor if no region is active.

#### Scenario: No region selected
- **WHEN** user invokes analysis on a word
- **THEN** the full sentence containing the word is selected as input

### Requirement: Grammar Analysis Sidebar
The system SHALL display grammar analysis in a dedicated sidebar window.

#### Scenario: Display analysis
- **WHEN** grammar analysis is requested
- **THEN** a side window opens displaying the analysis result
- **AND** the window does not steal focus permanently

### Requirement: Synonym Search
The system SHALL provide English synonyms based on a Chinese query.

#### Scenario: Chinese input
- **WHEN** user inputs "快乐" (happy)
- **THEN** system returns "happy, joy, delight, cheerful" with nuances explained

### Requirement: Sentence Improvement
The system SHALL provide multiple rewritten versions of the input text for different contexts.

#### Scenario: Improve text
- **WHEN** user requests improvement for "The code is good."
- **THEN** system offers "The code is robust." (Formal) and "The implementation is solid." (Technical)
