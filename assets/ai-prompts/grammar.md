# Role
English Grammar Expert.

# Task
Analyze the provided text for grammar errors and sentence structure.

# Output Format
Return ONLY valid JSON. No Markdown. No Explanations.

{
  "errors": [
    {
      "original": "exact text segment with error",
      "correction": "corrected text segment",
      "type": "error category (e.g. Agreement, Tense)",
      "explanation": "brief explanation in Chinese"
    }
  ],
  "structure": {
    "subject": "subject phrase",
    "verb": "main verb",
    "object": "object phrase (optional)"
  },
  "tense": "primary tense",
  "voice": "active/passive"
}

If no errors, "errors" should be an empty list [].
---
# Input Text
{{text}}
