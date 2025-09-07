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
(use-package llm
  :defer t
  :init
  (defcustom my/local-llm-provider nil
    ""
    :group 'my
    :type '(choice
            (sexp :tag "llm provider")
            (function :tag "Function that returns an llm provider.")))
  (defcustom my/gemini-llm-provider nil
    ""
    :group 'my
    :type '(choice
            (sexp :tag "llm provider")
            (function :tag "Function that returns an llm provider.")))
  :custom
  (llm-warn-on-nonfree nil))

(use-package gptel
  :ensure t
  :config
  (require 'gptel-integrations)
  (setq gptel-default-mode 'org-mode
        gptel-model 'gemma3:12b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(gemma3:12b))))

(use-package magit-gptcommit
  :straight t
  :demand t
  :after magit
  :config
  (magit-gptcommit-status-buffer-setup))

(use-package plz
  :defer t
  :custom
  (plz-curl-default-args `("--silent"
                           "--compressed"
                           "--location"
                           "-x"
                           ,(getenv "SOCKS5_PROXY"))))

;; (use-package mcp
;;   :ensure t
;;   :straight (mcp
;;              :host github
;;              :repo "lizqwerscott/mcp.el")
;;   :after gptel
;;   :custom
;;   (mcp-hub-servers
;;    `(("blender" .
;;       (:command "uvx"
;;                 :args ("blender-mcp")
;;                 :env (:HTTPS_PROXY ,(getenv "HTTPS_PROXY"))))))
;;   :config (require 'mcp-hub)
;;   :hook (after-init . mcp-hub-start-all-server))

(provide 'my-llm)

;;; my-llm.el ends here
