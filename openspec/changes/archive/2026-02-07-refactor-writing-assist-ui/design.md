## Context

Currently, the writing assistant tools (grammar, synonym, improve) in `lib/my-llm.el` render clickable buttons (`insert-text-button`) in the side window. This forces users to use the mouse or awkwardly navigate to buttons to trigger actions (like applying a fix). The goal is to move to a keyboard-centric interface where the side window acts as a navigable list of actionable items, similar to `flymake` or `grep` buffers.

## Goals / Non-Goals

**Goals:**
- Replace mouse-driven buttons with keyboard shortcuts in the side window.
- Implement `my-writing-assist-mode` for side buffers with standard navigation (`n`/`p`) and execution (`RET`) keys.
- Allow applying corrections, inserting synonyms, and replacing sentences via keyboard.
- Maintain the visual clarity of the existing side-window layout while making it "active".

**Non-Goals:**
- Changing the underlying LLM logic or prompts.
- Changing the overall side-window layout behavior (position, width).
- Implementing a main-buffer-driven interaction model (transient/hydra) at this stage.

## Decisions

### 1. Dedicated Minor Mode
- **Decision**: Define a new major mode `my-writing-assist-mode` derived from `special-mode` for all three writing assist buffers (`*my/grammar*`, `*my/synonym*`, `*my/improve*`).
- **Rationale**: `special-mode` provides read-only behavior and standard bindings (`q` to quit). Deriving a mode allows us to bind specific keys (`RET`, `n`, `p`) locally.

### 2. Text Properties for Metadata
- **Decision**: Instead of `insert-text-button` closures, use `text-properties` to attach metadata to the actionable text regions.
- **Properties**:
  - `my-writing-assist-action`: The type of action (e.g., `'apply-fix`, `'insert-synonym`, `'replace-sentence`).
  - `my-writing-assist-data`: A plist containing necessary data (e.g., `(:start 10 :end 20 :text "correction")`).
- **Rationale**: Decouples rendering from logic. Allows the `RET` command to inspect the text at point and dispatch the correct action.

### 3. Navigation
- **Decision**: Implement `my/writing-assist-next-item` and `my/writing-assist-prev-item` commands.
- **Mechanism**: Search for the `my-writing-assist-action` text property to jump between actionable items.
- **Number Selection**: Support executing items by number keys (`1`-`9`). Render numbered prefixes (e.g., `[1]`, `[2]`) next to actionable items.
- **Rationale**: Provides consistent navigation across different types of content (grammar errors vs synonym lists). Numbers offer faster direct access.

### 4. Rendering
- **Decision**: Remove `[Apply Fix]` buttons. Instead, make the relevant text (e.g., the correction suggestion or the synonym word) actionable.
- **Visuals**: Use faces (e.g., specific foreground color, or maybe a distinct background on hover/highlight) to indicate actionable items.

## Risks / Trade-offs

- **[Risk] Complexity**: Managing text properties and positions requires careful indexing, especially if the source buffer changes.
  - **Mitigation**: The current implementation already assumes source positions are stable during the interaction. This refactor doesn't change that assumption, but we must ensure we copy the necessary position data correctly into the text properties.
- **[Trade-off] Context Switching**: Users must switch focus to the side window to interact.
  - **Rationale**: Accepted trade-off for this iteration. It's standard Emacs behavior for "list" buffers.

## Migration Plan

1.  Define `my-writing-assist-mode`.
2.  Update `my/render-*` functions to use `propertize` with metadata instead of `insert-text-button`.
3.  Implement the action dispatcher command bound to `RET`.
4.  Bind `n`/`p` to navigation helpers.
