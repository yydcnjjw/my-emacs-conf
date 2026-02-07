## ADDED Requirements

### Requirement: Chinese-to-English Synonym Query
The system SHALL allow users to query English synonyms by inputting a Chinese term or definition.

#### Scenario: Querying via Minibuffer
- **WHEN** user triggers synonym lookup
- **THEN** system prompts for input in the minibuffer with "中文含义: "
- **AND** sends the input to the LLM to find relevant English synonyms

### Requirement: Structured Synonym Display
The system SHALL display synonym results in a structured format including the word, nuance explanation, formality level, and an example sentence.

#### Scenario: Viewing Synonyms
- **WHEN** synonym results are returned
- **THEN** results are displayed in `*my/synonym*` side buffer
- **AND** each entry shows the synonym word, its specific nuance (in Chinese), and an example usage

### Requirement: Insert Synonym
The system SHALL provide a mechanism to insert a selected synonym into the editing buffer at the cursor position.

#### Scenario: Inserting a Word
- **WHEN** user clicks "Insert" or activates an insertion action on a synonym
- **THEN** the selected word is inserted at point in the active buffer
