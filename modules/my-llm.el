;;; my-llm.el --- llm -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (ellama mcp)


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

(use-package gptel
  :ensure t
  :config
  (require 'gptel-integrations)
  (setq gptel-model 'gemma3:12b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(gemma3:12b))))

(use-package magit-gptcommit
  :straight t
  :demand t
  :after magit
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :init
  (require 'llm-ollama)
  :custom
  (magit-gptcommit-llm-provider
   (make-llm-ollama
    :embedding-model "gemma3:12b"
    :chat-model "gemma3:12b"
    :default-chat-temperature 0.1))

  :config
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))

(use-package mcp
  :ensure t
  :straight (mcp
             :host github
             :repo "lizqwerscott/mcp.el")
  :after gptel
  :custom (mcp-hub-servers
           `(("blender" . (:command "uvx" :args ("blender-mcp")))))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))

(provide 'my-llm)

;;; my-llm.el ends here
