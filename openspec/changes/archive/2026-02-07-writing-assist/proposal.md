## Why

Enhance the English writing workflow within Emacs by providing integrated, LLM-powered tools for grammar checking, synonym discovery, and sentence polishing. This addresses the need for context-aware, high-quality writing assistance directly within the editor, leveraging the existing `my-llm` infrastructure to provide a seamless and interactive experience without leaving the Emacs environment.

## What Changes

- **Grammar Analysis**: Add `my/grammar-check` to automatically select the current sentence (intelligent selection based on mode), analyze it via LLM, and display errors/structural breakdown in a side sidebar.
- **Synonym Lookup**: Add `my/synonym-lookup` allowing users to query English synonyms using Chinese input, presenting results with nuanced explanations and usage examples.
- **Sentence Improvement**: Add `my/sentence-improve` to offer rewriting suggestions based on selectable contexts (Formal, Casual, Academic, Business, Simple, Blog, Diary).
- **Unified UI Architecture**: Implement a shared side-window architecture for these tools, utilizing independent buffers (`*my/grammar*`, `*my/synonym*`, `*my/improve*`).
- **Streaming JSON Handling**: Implement a robust mechanism for handling streaming LLM output that parses JSON upon completion to render rich, interactive UIs.

## Capabilities

### New Capabilities
- `grammar-analysis`: Grammar checking with mode-aware auto-selection and interactive error correction.
- `synonym-lookup`: Context-rich Chinese-to-English synonym discovery with structured output.
- `sentence-improvement`: Scenario-based sentence rewriting and polishing.

### Modified Capabilities
<!-- No existing specs are being modified by this change -->

## Impact

- **Codebase**:
  - `lib/my-llm.el`: Significant expansion to include new feature logic, prompts, and UI rendering code.
  - `config/init-llm.el`: Potential updates if new dependencies are introduced.
- **User Experience**: New side-window panels will appear during writing tasks. New entries in `my/language-tool-menu`.
- **Performance**: LLM interaction adds network/processing latency; "loading" states will be managed via UI indicators.
