## Why

The current implementation of the writing assistant tool relies on mouse-driven interaction (clicking "Apply Fix" buttons), which disrupts the keyboard-centric workflow favored by Emacs users. Refactoring the UI to support keyboard navigation and execution within the side window will provide a more idiomatic, efficient, and seamless user experience.

## What Changes

- **Remove Buttons**: Eliminate `insert-text-button` from `my/render-*` functions.
- **Implement Side-Window Mode**: Introduce `my-writing-assist-mode` (derived from `special-mode`) for the side buffers.
- **Add Keybindings**:
  - `n`/`p`: Navigate between suggestions/errors.
  - `RET`: Execute the action (Apply Fix, Insert Synonym, Replace Sentence) for the item at point.
  - `q`: Quit the window.
- **Store Metadata**: Attach necessary action data (replacement text, positions) as text properties on the rendered items instead of using button closures.

## Capabilities

### New Capabilities
<!-- No new capabilities, strictly refactoring existing interaction -->

### Modified Capabilities
- `grammar-analysis`: UI interaction changes from button-click to keyboard-driven selection and application.
- `synonym-lookup`: UI interaction changes from button-click to keyboard-driven insertion.
- `sentence-improvement`: UI interaction changes from button-click to keyboard-driven replacement.

## Impact

- **Codebase**:
  - `lib/my-llm.el`: Significant changes to rendering logic and addition of the new mode and keymap.
- **User Experience**: Users will now need to switch focus to the side window (or have it auto-focused) and use keys to interact, rather than clicking.
