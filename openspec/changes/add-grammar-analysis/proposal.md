# Change: Add Grammar Analysis Tools

## Why
Users need quick access to linguistic tools while writing to improve the quality of their English text. Currently, simple translation is supported, but in-depth grammar analysis, synonym lookup, and stylistic improvements require external tools or context switching.

## What Changes
- **Add Grammar Analysis:** Automate sentence selection and provide detailed grammar breakdown in a sidebar.
- **Add Synonym Search:** Allow querying English synonyms using Chinese input.
- **Add Sentence Improvement:** Generate multiple rewriting suggestions based on different contexts (e.g., formal, academic).
- Integrate these tools into the existing LLM infrastructure (`my-llm.el`).

## Impact
- **Affected Specs:** New capability `grammar-analysis`.
- **Affected Code:** `lib/my-llm.el`, `config/init-llm.el`.
