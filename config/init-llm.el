;;; init-llm.el --- llm -*- lexical-binding: t -*-

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

(require 'my-path)

(use-package plz
  :defer t
  :config
  (setopt plz-curl-default-args
          `("--silent"
            "--compressed"
            "--location"
            ,@(when-let (proxy (getenv "ALL_PROXY")) (list "-x" proxy)))))

(use-package llm
  :straight (:type git :host github
                   :repo "yydcnjjw/llm"
                   :branch "main")
  :defer t
  :config
  (setopt llm-warn-on-nonfree nil))

(use-package ai-code
  :straight (:type git :host github
                   :repo "tninja/ai-code-interface.el"
                   :files ("*.el" "snippets"))
  :defer t
  :init
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients))
  :config
  (require 'ai-code-gemini-cli)
  (ai-code-set-backend 'gemini))

(use-package emacs
  :ensure separedit
  :bind
  ("C-c o" . my/ai-menu)
  :config
  (require 'my-llm))

(use-package minuet
  :bind
  (("M-y" . minuet-complete-with-minibuffer)
   ("M-i" . minuet-show-suggestion)
   :map minuet-active-mode-map
   ("M-p" . minuet-previous-suggestion)
   ("M-n" . minuet-next-suggestion)
   ("M-A" . minuet-accept-suggestion)
   ("M-a" . minuet-accept-suggestion-line)
   ("M-e" . minuet-dismiss-suggestion))
  :functions minuet-set-optional-options
  :config
  (setopt minuet-provider 'openai-fim-compatible
          minuet-n-completions 1
          minuet-context-window 512)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://127.0.0.1:11434/v1/completions")
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))

(provide 'init-llm)

;;; init-llm.el ends here
