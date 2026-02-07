## ADDED Requirements

### Requirement: Scenario-Based Improvement
The system SHALL offer sentence improvement suggestions tailored to specific user-selected contexts/scenarios.

#### Scenario: Selecting a Scenario
- **WHEN** user triggers sentence improvement
- **THEN** system presents a menu of scenarios: Formal, Casual, Academic, Business, Simple, Blog, Diary
- **AND** uses the selected scenario to guide the LLM's rewriting suggestions

### Requirement: Multiple Improvement Options
The system SHALL provide multiple distinct rewriting options for a single sentence, each with a brief explanation of the change.

#### Scenario: Viewing Suggestions
- **WHEN** analysis is complete
- **THEN** `*my/improve*` buffer displays multiple rewritten versions of the original sentence
- **AND** each version includes a label (e.g., "Concise", "Polite") explaining the nature of the improvement

### Requirement: Replace Original Sentence
The system SHALL allow users to replace the original sentence with a selected improved version.

#### Scenario: Replacing Text
- **WHEN** user confirms a suggestion
- **THEN** the original sentence in the buffer is replaced by the selected improved version
