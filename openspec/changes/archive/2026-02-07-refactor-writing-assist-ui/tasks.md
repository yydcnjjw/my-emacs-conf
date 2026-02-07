## 1. Setup and Infrastructure

- [x] 1.1 Define `my-writing-assist-mode` in `lib/my-llm-ui.el` (or `my-llm.el`), inheriting from `special-mode`.
- [x] 1.2 Define keymap for `my-writing-assist-mode` (RET, n, p, q, 1-9).
- [x] 1.3 Implement navigation helper functions (`my/writing-assist-next-item`, `my/writing-assist-prev-item`).
- [x] 1.4 Implement execution helper function (`my/writing-assist-execute-action` bound to RET).
- [x] 1.5 Implement number execution helper (`my/writing-assist-execute-number` bound to 1-9).

## 2. Refactor Renderers

- [x] 2.1 Refactor `my/render-grammar-result` to use text properties instead of buttons.
- [x] 2.2 Refactor `my/render-synonym-result` to use text properties instead of buttons.
- [x] 2.3 Refactor `my/render-improve-result` to use text properties instead of buttons.
- [x] 2.4 Ensure all renderers apply `my-writing-assist-mode` to the buffer.

## 3. Integration and Cleanup

- [ ] 3.1 Verify navigation works (n/p moves between items).
- [ ] 3.2 Verify execution works (RET triggers action).
- [ ] 3.3 Verify number keys work (1-9 trigger corresponding item).
- [ ] 3.4 Verify clean display (no button artifacts).
