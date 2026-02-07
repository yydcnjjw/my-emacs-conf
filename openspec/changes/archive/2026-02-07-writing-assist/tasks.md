## 1. Setup and Infrastructure

- [x] 1.1 Create `my-llm-ui.el` for unified side-window logic (display-buffer-in-side-window)
- [x] 1.2 Implement `my/llm-streaming-json` helper (streams text, parses JSON on completion)
- [x] 1.3 Update `my/ai-menu` in `lib/my-llm.el` to include new tools

## 2. Grammar Analysis Implementation

- [x] 2.1 Implement smart sentence selection logic (`my/select-sentence-at-point`)
- [x] 2.2 Define Grammar Analysis prompt (JSON schema)
- [x] 2.3 Create `my/grammar-check` interactive function
- [x] 2.4 Implement grammar results renderer (errors, structure, Apply Fix button)

## 3. Synonym Lookup Implementation

- [x] 3.1 Define Synonym Lookup prompt (JSON schema)
- [x] 3.2 Create `my/synonym-lookup` interactive function (minibuffer input)
- [x] 3.3 Implement synonym results renderer (list, nuances, Insert button)

## 4. Sentence Improvement Implementation

- [x] 4.1 Define Sentence Improvement prompt (JSON schema)
- [x] 4.2 Create `transient` menu for scenario selection (Formal, Casual, etc.)
- [x] 4.3 Create `my/sentence-improve` interactive function
- [x] 4.4 Implement improvement results renderer (suggestions, Replace button)

## 5. Integration and Testing

- [x] 5.1 Verify all tools work in `org-mode`
- [x] 5.2 Verify all tools work in `markdown-mode`
- [x] 5.3 Verify grammar check works in `prog-mode` comments
- [x] 5.4 Test JSON parsing resilience with local vs cloud models
