;;; my-llm.el --- llm -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'separedit)
(require 'transient)
(require 'alert)
(require 'subr-x)
(require 'thingatpt)
(require 'my-llm-ui)
(require 'markdown-mode)
(require 'json)
(require 'gptel-prompts)

;;; Customization

;; (defcustom my/local-default-chat-model "gpt-oss:20b"
;;   "Local default chat model."
;;   :group 'my
;;   :type 'string)

;; (defcustom my/local-default-embedding-model "qwen3-embedding:8b"
;;   "Local default chat model."
;;   :group 'my
;;   :type 'string)

;; (defcustom my/cloud-default-chat-model "gemini-2.5-flash"
;;   "Cloud default chat model."
;;   :group 'my
;;   :type 'string)

;; (defcustom my/cloud-default-embedding-model nil
;;   "Cloud default embedding model."
;;   :group 'my
;;   :type 'string)

;; (defcustom my/cloud-api-key ""
;;   "Cloud api key."
;;   :group 'my
;;   :type 'string)

;; (defcustom my/local-llm-provider (lambda ()
;;                                    (make-llm-ollama
;;                                     :host "127.0.0.1"
;;                                     :embedding-model my/local-default-embedding-model
;;                                     :chat-model my/local-default-chat-model))
;;   "Local llm provider."
;;   :group 'my
;;   :type '(choice
;;           (sexp :tag "llm provider")
;;           (function :tag "Function that returns an llm provider.")))

;; (defcustom my/local-llm-extra-params
;;   (list
;;    (cons 'num_ctx (llm-model-context-length (llm-models-match my/local-default-chat-model))))
;;   "Local llm extra params."
;;   :group 'my
;;   :type 'alist)

;; (defcustom my/cloud-llm-provider (lambda ()
;;                                    (make-llm-gemini
;;                                     :key my/cloud-api-key
;;                                     ;; :embedding-model (or my/cloud-default-embedding-model "embedding-001")
;;                                     :chat-model my/cloud-default-chat-model))
;;   "Cloud llm provider."
;;   :group 'my
;;   :type '(choice
;;           (sexp :tag "llm provider")
;;           (function :tag "Function that returns an llm provider.")))

;; (defcustom my/translate-llm-provider my/local-llm-provider
;;   "Translate llm provider."
;;   :group 'my
;;   :type '(choice
;;           (sexp :tag "llm provider")
;;           (function :tag "Function that returns an llm provider.")))

;;; Internal Variables

(defvar my/translate-buffer "*translate*")
(defvar my/translate-directive (alist-get 'translate gptel-directives))

(defvar my/block-info-major-modes
  '(markdown-mode
    org-mode
    gfm-mode
    mu4e-view-mode
    elfeed-show-mode
    fundamental-mode
    help-mode))

(defun my/current-buffer-block-info (&optional in-place)
  "Current buffer block info with IN-PLACE."
  (cond
   ((use-region-p)
    (list :beginning (region-beginning)
          :end (region-end)
          :major-mode 'fundamental-mode))
   ((and (not in-place) (memq major-mode my/block-info-major-modes))
    (list :beginning (point-min)
          :end (point-max)
          :major-mode major-mode))
   (t (separedit--block-info))))

(defun my/translate-rewrite-dwim ()
  "Rewrite text to translate text."
  (interactive)
  
  (gptel-request my/translate-directive
    :buffer my/translate-buffer
    :stream t
    )
  
  (let* ((source-buffer (current-buffer))
         (selection (my/select-sentence-at-point))
         (text (nth 0 selection))
         (beg (nth 1 selection))
         (end (nth 2 selection))
         (prompt-data (my/llm-prompt-render "translate" `((text . ,text) (language . "English")))))
    (my/apply-text-replacement source-buffer beg end "") ; Prepare the area
    (goto-char beg)
    (llm-chat-streaming-to-point
     (my/get-llm-provider my/translate-llm-provider)
     (llm-make-chat-prompt (plist-get prompt-data :user)
                           :context (plist-get prompt-data :system)
                           :reasoning 'light
                           :non-standard-params my/local-llm-extra-params)
     source-buffer beg (lambda ()))))

(defun my/translate-dwim ()
  "Translate dwim."
  (interactive)
  (let* ((_parent-buffer (current-buffer))
         (block (my/current-buffer-block-info))
         (beg (plist-get block :beginning))
         (end (plist-get block :end))
         (block-major-mode (plist-get block :major-mode))
         (parent-buffer-read-only buffer-read-only)
         (content (buffer-substring-no-properties beg end)))
    (with-current-buffer (get-buffer-create my/translate-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if parent-buffer-read-only
            (funcall 'fundamental-mode)
          (funcall (or block-major-mode 'fundamental-mode)))
        (setq-local gptel-prompts-template-variables
                    `(("source_language" . "English")
                      ("target_language" . "Simple Chinese")))
        (gptel-request content
          :system (gptel-prompts-interpolate (alist-get 'translate gptel-directives))
          :stream t)
        (display-buffer (current-buffer))))))

(defun my/dictionary-query-word ()
  "Query word from AI."
  (interactive)
  (let* ((word (word-at-point))
         (buffer (or (get-buffer my/dictionary-buffer-name)
                     (generate-new-buffer my/dictionary-buffer-name)))
         (prompt-data (my/llm-prompt-render "dictionary" `((word . ,word)))))
    (with-current-buffer buffer
      (erase-buffer)
      ;; (read-only-mode)
      (gfm-mode)
      (when my/dictionary-llm-request
        (llm-cancel-request my/dictionary-llm-request))
      (setq my/dictionary-llm-request
            (llm-chat-streaming-to-point
             (my/get-llm-provider my/translate-llm-provider)
             (llm-make-chat-prompt (plist-get prompt-data :user)
                                   :context (plist-get prompt-data :system)
                                   :reasoning 'none
                                   :non-standard-params my/local-llm-extra-params)
             buffer (point-max)
             (lambda ()))))
    (display-buffer buffer)))

(defun my/synonym-lookup (term)
  "Lookup synonyms for TERM (Chinese)."
  (interactive "s中文含义: ")
  (let ((buffer (my/llm-ui-get-buffer-create my/llm-ui-buffer-name-synonym))
        (target-buffer (current-buffer))
        (prompt-data (my/llm-prompt-render "synonym" `((term . ,term)))))
    
    (my/llm-ui-display-buffer-in-side-window buffer)
    (my/llm-ui-render-loading buffer (format "Finding synonyms for '%s'" term))
    
    (my/llm-streaming-json
     (my/get-llm-provider my/translate-llm-provider)
     (llm-make-chat-prompt (plist-get prompt-data :user)
                           :context (plist-get prompt-data :system)
                           :reasoning 'none
                           :non-standard-params my/local-llm-extra-params)
     buffer
     (lambda (data)
       (my/render-synonym-result buffer target-buffer data))
     (lambda (msg)
       (with-current-buffer buffer
         (insert (format "\n\n❌ Error: %s" msg)))))))

(defun my/grammar-check ()
  "Check grammar for sentence at point."
  (interactive)
  (let* ((selection (my/select-sentence-at-point))
         (text (nth 0 selection))
         (beg (nth 1 selection))
         (end (nth 2 selection))
         (source-buffer (current-buffer))
         (buffer (my/llm-ui-get-buffer-create my/llm-ui-buffer-name-grammar))
         (prompt-data (my/llm-prompt-render "grammar" `((text . ,text)))))
    (unless (and text (not (string-blank-p text)))
      (user-error "No sentence found at point"))
    
    (my/llm-ui-display-buffer-in-side-window buffer)
    (my/llm-ui-render-loading buffer "Analyzing Grammar")
    
    (my/llm-streaming-json
     (my/get-llm-provider my/translate-llm-provider)
     (llm-make-chat-prompt (plist-get prompt-data :user)
                           :context (plist-get prompt-data :system)
                           :reasoning 'none
                           :non-standard-params my/local-llm-extra-params)
     buffer
     (lambda (data)
       (my/render-grammar-result buffer source-buffer beg end data))
     (lambda (msg)
       (with-current-buffer buffer
         (let ((inhibit-read-only t))
           (insert (format "\n\n❌ Error: %s" msg))))))))

(defun my/sentence-improve (scenario)
  "Improve sentence with SCENARIO."
  (interactive "sScenario: ")
  (let* ((selection (my/select-sentence-at-point))
         (text (nth 0 selection))
         (beg (nth 1 selection))
         (end (nth 2 selection))
         (source-buffer (current-buffer))
         (buffer (my/llm-ui-get-buffer-create my/llm-ui-buffer-name-improve))
         (prompt-data (my/llm-prompt-render "improve" `((text . ,text) (scenario . ,scenario)))))
    (unless (and text (not (string-blank-p text)))
      (user-error "No sentence found at point"))
    
    (my/llm-ui-display-buffer-in-side-window buffer)
    (my/llm-ui-render-loading buffer (format "Improving (%s)" scenario))
    
    (my/llm-streaming-json
     (my/get-llm-provider my/translate-llm-provider)
     (llm-make-chat-prompt (plist-get prompt-data :user)
                           :context (plist-get prompt-data :system)
                           :reasoning 'none
                           :non-standard-params my/local-llm-extra-params)
     buffer
     (lambda (data)
       (my/render-improve-result buffer source-buffer beg end data))
      (lambda (msg)
       (with-current-buffer buffer
         (insert (format "\n\n❌ Error: %s" msg)))))))

;;; UI Rendering

(defun my/render-improve-result (buffer source-buffer start-pos end-pos data)
  "Render improvement DATA in BUFFER.
SOURCE-BUFFER is the buffer where text will be replaced.
START-POS and END-POS define the region to replace."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((original (alist-get 'original data))
            (suggestions (alist-get 'suggestions data))
            (count 0))
        
        (insert (propertize "\n  句子润色 (Sentence Improvement)\n" 'face '(:height 1.2 :weight bold)))
        (insert (propertize "  ───────────────────────────────\n\n" 'face '(:foreground "gray")))
        
        (insert (propertize "  原文 (Original):\n" 'face '(:weight bold)))
        (insert (format "  %s\n\n" (propertize original 'face '(:slant italic))))
        
        (if (seq-empty-p suggestions)
            (insert "  未找到建议 (No suggestions found).")
          (insert (propertize "  建议 (Suggestions):\n" 'face '(:weight bold)))
          (seq-do (lambda (item)
                    (setq count (1+ count))
                    (let ((text (alist-get 'text item))
                          (label (alist-get 'label item))
                          (explanation (alist-get 'explanation item)))
                      (insert (format "  [%d] " count))
                      (let ((act-start (point)))
                        (insert (format "%s" (propertize label 'face '(:weight bold :foreground "cyan"))))
                        (insert (format " (%s)\n" explanation))
                        (insert (format "      \"%s\"" text))
                        (add-text-properties act-start (point)
                                             `(my-writing-assist-action
                                                (lambda (d)
                                                  (my/apply-text-replacement
                                                   (plist-get d :source-buffer)
                                                   (plist-get d :start-pos)
                                                   (plist-get d :end-pos)
                                                   (plist-get d :text))
                                                  (message "Replaced with: %s" (plist-get d :label)))
                                                my-writing-assist-data
                                                (:source-buffer ,source-buffer
                                                 :start-pos ,start-pos
                                                 :end-pos ,end-pos
                                                 :text ,text
                                                 :label ,label)
                                                mouse-face highlight
                                                help-echo "RET to replace")))
                      (insert "\n\n")))
                  suggestions))
        (goto-char (point-min))))))

(defun my/render-synonym-result (buffer target-buffer data)
  "Render synonym DATA in BUFFER.
TARGET-BUFFER is where the selected synonym will be inserted."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((input (alist-get 'input data))
            (synonyms (alist-get 'synonyms data))
            (count 0))
        
        (insert (propertize "\n  同义词查询 (Synonym Lookup)\n" 'face '(:height 1.2 :weight bold)))
        (insert (format "  查询 (Query): %s\n" input))
        (insert (propertize "  ───────────────────────────\n\n" 'face '(:foreground "gray")))
        
        (if (seq-empty-p synonyms)
            (insert "  未找到同义词 (No synonyms found).")
          (seq-do (lambda (item)
                    (setq count (1+ count))
                    (let ((word (alist-get 'word item))
                          (nuance (alist-get 'nuance item))
                          (formality (alist-get 'formality item))
                          (example (alist-get 'example item)))
                      (insert (format "  [%d] " count))
                      (let ((act-start (point)))
                        (insert (format "%s " (propertize word 'face '(:weight bold :foreground "cyan"))))
                        (insert (propertize (format "[%s]\n" formality) 'face '(:height 0.8 :foreground "gray")))
                        (insert (format "      %s\n" nuance))
                        (insert (format "      例句: \"%s\"" (propertize example 'face '(:slant italic))))
                        (add-text-properties act-start (point)
                                             `(my-writing-assist-action
                                                (lambda (d)
                                                  (with-current-buffer (plist-get d :target-buffer)
                                                    (insert (plist-get d :word)))
                                                  (message "Inserted: %s" (plist-get d :word)))
                                                my-writing-assist-data
                                                (:target-buffer ,target-buffer
                                                 :word ,word)
                                                mouse-face highlight
                                                help-echo "RET to insert")))
                      (insert "\n\n")))
                  synonyms))
        (goto-char (point-min))))))

(defun my/render-grammar-result (buffer source-buffer start-pos end-pos data)
  "Render grammar analysis DATA in BUFFER.
SOURCE-BUFFER is the original buffer.
START-POS and END-POS define the sentence bounds."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((errors (alist-get 'errors data))
            (structure (alist-get 'structure data))
            (tense (alist-get 'tense data))
            (voice (alist-get 'voice data))
            (count 0))
        
        (insert (propertize "\n  语法分析 (Grammar Analysis)\n" 'face '(:height 1.2 :weight bold)))
        (insert (propertize "  ────────────────\n\n" 'face '(:foreground "gray")))
        
        ;; 1. Structure
        (when structure
          (insert (propertize "  句子结构 (Sentence Structure):\n" 'face '(:weight bold)))
          (insert (format "  • 主语 (Subject): %s\n" (alist-get 'subject structure)))
          (insert (format "  • 谓语 (Verb):    %s\n" (alist-get 'verb structure)))
          (when (alist-get 'object structure)
            (insert (format "  • 宾语 (Object):  %s\n" (alist-get 'object structure))))
          (insert (format "  • 时态 (Tense):   %s\n" tense))
          (insert (format "  • 语态 (Voice):   %s\n\n" voice)))
        
        ;; 2. Errors
        (if (seq-empty-p errors)
            (insert (propertize "  ✅ 未发现错误 (No errors found)." 'face '(:foreground "green")))
          (insert (propertize "  发现问题 (Found Issues):\n" 'face '(:weight bold :foreground "orange")))
          (seq-do (lambda (err)
                    (setq count (1+ count))
                    (let ((original (alist-get 'original err))
                          (correction (alist-get 'correction err))
                          (type (alist-get 'type err))
                          (explanation (alist-get 'explanation err)))
                      (insert (format "  • [%s] %s\n" type explanation))
                      (insert (format "    原文: \"%s\"\n" (propertize original 'face '(:strike-through t))))
                      (insert (format "    [%d] 修正: " count))
                      (let ((act-start (point)))
                        (insert (format "\"%s\"" (propertize correction 'face '(:weight bold))))
                        (add-text-properties act-start (point)
                                             `(my-writing-assist-action
                                                (lambda (d)
                                                  (with-current-buffer (plist-get d :source-buffer)
                                                    (save-excursion
                                                      (goto-char (plist-get d :start-pos))
                                                      ;; Search within the sentence bounds
                                                      (let ((limit (plist-get d :end-pos)))
                                                        (if (search-forward (plist-get d :original) limit t)
                                                            (replace-match (plist-get d :correction))
                                                          (message "Could not find original text")))))
                                                  (message "Applied fix: %s -> %s" (plist-get d :original) (plist-get d :correction)))
                                                my-writing-assist-data
                                                (:source-buffer ,source-buffer
                                                 :start-pos ,start-pos
                                                 :end-pos ,end-pos
                                                 :original ,original
                                                 :correction ,correction)
                                                mouse-face highlight
                                                help-echo "RET to apply fix")))
                      (insert "\n\n")))
                  errors))
        (goto-char (point-min))))))

;;; Model Selection UI

(defun my/completing-read-local-models ()
  "Completing read local models."
  (completing-read "Local models: " (llm-models (my/get-llm-provider my/local-llm-provider))))

(defun my/select-local-chat-model ()
  "Select local chat model."
  (interactive)
  (let ((model (my/completing-read-local-models)))
    (setopt my/local-default-chat-model model)))

(defun my/completing-read-cloud-models ()
  "Completing read cloud models."
  (completing-read "Cloud models: " (llm-models (my/get-llm-provider my/cloud-llm-provider))))

(defun my/select-cloud-chat-model ()
  "Select cloud chat model."
  (interactive)
  (let ((model (my/completing-read-cloud-models)))
    (setopt my/cloud-default-chat-model model)))

(defun my/select-provider ()
  "Select provider."
  (interactive)
  (completing-read "Providers: " (list 'my/local-llm-provider 'my/cloud-llm-provider)))

(defun my/select-translate-provider ()
  "Select translate provider."
  (interactive)
  (setopt my/translate-llm-provider (symbol-value (intern (my/select-provider)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menus

(transient-define-prefix my/sentence-improve-menu ()
  "Select scenario for sentence improvement."
  ["Select Scenario"
   [("f" "Formal"       (lambda () (interactive) (my/sentence-improve "Formal")))
    ("c" "Casual"       (lambda () (interactive) (my/sentence-improve "Casual")))
    ("a" "Academic"     (lambda () (interactive) (my/sentence-improve "Academic")))]
   [("b" "Business"     (lambda () (interactive) (my/sentence-improve "Business")))
    ("s" "Simple"       (lambda () (interactive) (my/sentence-improve "Simple")))
    ("l" "Blog"         (lambda () (interactive) (my/sentence-improve "Blog")))
    ("d" "Diary"        (lambda () (interactive) (my/sentence-improve "Diary")))]])

(transient-define-prefix my/language-tool-menu ()
  "Language tool Menu."
  ["Language tool Commands"
   ["AI Translate"
    ("t" "Translate and show in buffer." my/translate-dwim)
    ("c" "Translate and replace" my/translate-change-dwim)]
   ["AI Dictionary"
    ("d" "Query word in side buffer." my/dictionary-query-word)]
   ["Writing Assist"
    ("g" "Grammar Check" my/grammar-check)
    ("s" "Synonym Lookup" my/synonym-lookup)
    ("i" "Sentence Improve" my/sentence-improve-menu)]])

(transient-define-prefix my/ai-settings-menu ()
  "AI settings Menu."
  ["AI Settings"
   [("l" "Select local chat model" my/select-local-chat-model)
    ("c" "Select cloud chat model" my/select-cloud-chat-model)
    ("t" "Select translate provider" my/select-translate-provider)]])

(transient-define-prefix my/ai-menu ()
  "AI Menu."
  ["AI tools"
   [("c" "Opencode" opencode)
    ("l" "Language tool" my/language-tool-menu)
    ("s" "Settings" my/ai-settings-menu)]]
  (interactive)
  (transient-setup 'my/ai-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alerts & Archived

(defun my/ai-agent-alert (title message)
  "Display notification with TITLE and MESSAGE using the `alert'."
  (alert message :title title :category 'agenda))

;;; ARCHIVED
;; (use-package ai-code
;;   :straight (:type git :host github
;;                    :repo "tninja/ai-code-interface.el"
;;                    :files ("*.el" "snippets"))
;;   :defer t
;;   :init
;;   (with-eval-after-load 'magit
;;     (ai-code-magit-setup-transients))
;;   :config
;;   (require 'ai-code-gemini-cli)
;;   (ai-code-set-backend 'gemini))

;; (use-package minuet
;;   :bind
;;   (("M-y" . minuet-complete-with-minibuffer)
;;    ("M-i" . minuet-show-suggestion)
;;    :map minuet-active-mode-map
;;    ("M-p" . minuet-previous-suggestion)
;;    ("M-n" . minuet-next-suggestion)
;;    ("M-A" . minuet-accept-suggestion)
;;    ("M-a" . minuet-accept-suggestion-line)
;;    ("M-e" . minuet-dismiss-suggestion))
;;   :functions minuet-set-optional-options
;;   :config
;;   (setopt minuet-provider 'openai-fim-compatible
;;           minuet-n-completions 1
;;           minuet-context-window 512)
;;   (plist-put minuet-openai-fim-compatible-options :end-point "http://127.0.0.1:11434/v1/completions")
;;   (plist-put minuet-openai-fim-compatible-options :name "Ollama")
;;   (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
;;   (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")

;;   (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))

(provide 'my-llm)

;;; my-llm.el ends here
