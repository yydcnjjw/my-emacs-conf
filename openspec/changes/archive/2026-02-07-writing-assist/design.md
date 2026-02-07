## Context

The current `my-llm` module provides basic translation and dictionary lookup capabilities using streaming LLM responses directly into buffers (`*my/translate*`, `*my/dictionary*`). These tools use `display-buffer` with default placement, resulting in unpredictable window management. The new requirement is to add three advanced writing tools—Grammar Analysis, Synonym Lookup, and Sentence Improvement—that require structured data (JSON) from the LLM to support rich, interactive UIs (e.g., clickable corrections, organized lists). The goal is to integrate these into a unified, predictable side-window interface.

## Goals / Non-Goals

**Goals:**
- Implement three new AI-driven features: Grammar Analysis, Synonym Lookup, and Sentence Improvement.
- Establish a unified side-window architecture for all writing assistance tools.
- Implement a reliable "Streaming JSON" pattern: stream raw output for feedback -> parse on completion -> render rich UI.
- Provide mode-aware sentence selection (Org, Markdown, Prog-mode) for grammar analysis.
- Ensure all new features are accessible via `my/language-tool-menu`.

**Non-Goals:**
- Real-time/as-you-type grammar checking (too resource-intensive/distracting).
- Replacing the existing `my/translate` or `my/dictionary` implementation immediately (though they may migrate to this pattern later).
- Offline grammar checking (depends on LLM).

## Decisions

### 1. Unified Side-Window Architecture
- **Decision**: Use `display-buffer-in-side-window` with `(side . right)` and `(window-width . 0.35)` for all three tools.
- **Rationale**: Provides a consistent, predictable location for auxiliary tools, preventing them from obscuring the main writing area.
- **Buffers**: Use separate buffers (`*my/grammar*`, `*my/synonym*`, `*my/improve*`) but reuse the same side-window slot (`slot . 0`). This maintains state for each tool independently while keeping the UI clean.

### 2. Interaction Pattern: Streaming JSON with Delayed Rendering
- **Decision**: The LLM will stream a JSON string. During streaming, the buffer will display a "Loading/Analyzing..." indicator (and optionally the raw stream for debugging). Upon stream completion (`:on-success` callback), the JSON will be parsed, and the buffer content will be replaced with a rendered, interactive UI (buttons, formatting).
- **Rationale**: LLM responses can be slow. Streaming provides immediate feedback that "something is happening." Parsing only at the end avoids the complexity of parsing partial JSON. JSON enables structured UIs (lists with metadata, actionable buttons) which raw text streams cannot easily support.

### 3. Feature-Specific Logic
- **Grammar Analysis**:
  - **Selection**: "Smart selection" strategy. If a region is active, use it. If not:
    - `org-mode`/`markdown-mode`: Use `thing-at-point 'sentence`.
    - `prog-mode`: Use `separedit` to extract comment block, then find sentence within.
  - **Output**: JSON containing errors (with fix replacements) and sentence structure breakdown.
  - **Action**: "Apply Fix" button to verify and replace text in original buffer.
- **Synonym Lookup**:
  - **Input**: `read-string` from minibuffer (prompt: "中文含义: ").
  - **Output**: JSON list of synonyms with nuance, formality, and examples.
  - **Action**: "Insert" button to insert word at cursor.
- **Sentence Improvement**:
  - **Input**: Current sentence (same selection logic as Grammar) + Scenario selected via `transient`.
  - **Scenarios**: Formal, Casual, Academic, Business, Simple, Blog, Diary.
  - **Output**: JSON list of suggestions with explanations.
  - **Action**: "Replace" button.

### 4. Prompt Engineering
- **Decision**: Use `json-mode` or strictly instructed JSON prompts.
- **Rationale**: Essential for reliable parsing. Prompts must explicitly forbid markdown code blocks (```json) to simplify parsing, or the parser must strip them.

## Risks / Trade-offs

- **[Risk] JSON Parsing Failure**: LLMs might output malformed JSON or interrupt mid-stream.
  - **Mitigation**: Implement robust error handling in the callback. If parsing fails, display the raw text and an error message so the user can still read the output.
- **[Risk] Sentence Selection Accuracy**: `thing-at-point 'sentence` in Org-mode can be flaky with links/formatting.
  - **Mitigation**: Accept "good enough" for v1. Users can always manually select a region if auto-selection fails.
- **[Risk] Latency**: Waiting for full JSON before rendering might feel slower than raw text streaming.
  - **Mitigation**: High-quality "Loading" animation or status messages.
