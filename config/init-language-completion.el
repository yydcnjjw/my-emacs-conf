;;; init-language-completion.el --- language-completion -*- lexical-binding: t -*-

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

(use-package emacs
  :init
  (setq-default read-process-output-max (* 1024 1024)))

(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-mode
  :defer t
  :functions
  (my/lsp-booster)
  :commands
  (lsp)
  :hook
  ((lsp-completion-mode . my/lsp-completion-mode))
  :config
  (setopt lsp-keymap-prefix "C-c l"
          lsp-completion-provider :none
          lsp-auto-execute-action nil
          lsp-keep-workspace-alive nil
          lsp-session-file (expand-file-name ".lsp-session-v1" my/emacs-cache-dir)
          read-process-output-max (* 1024 1024 5))
  (require 'my-lsp-mode)
  (my/lsp-booster))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  (setopt lsp-ui-doc-show-with-cursor t))

(use-package yasnippet
  :config
  (setopt yas-snippet-dirs (list (expand-file-name "snippets" my/emacs-assets-dir)))
  :hook
  ((prog-mode . yas-minor-mode)
   (text-mode . yas-minor-mode))
  :commands
  (yas-insert-snippet)
  :bind
  ("C-c y" . #'yas-insert-snippet))

(use-package yasnippet-snippets
  :after (yasnippet))

(provide 'init-language-completion)

;;; init-language-completion.el ends here
