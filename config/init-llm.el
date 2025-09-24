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

(use-package llm
  :ensure separedit
  :bind
  (("C-c t" . my/translate-dwim))
  :init
  (setopt llm-warn-on-nonfree nil)
  :config
  (require 'my-llm))

(use-package plz
  :defer t
  :init
  (setopt plz-curl-default-args `("--silent"
                                  "--compressed"
                                  "--location"
                                  "-x"
                                  ,(getenv "SOCKS5_PROXY"))))

(use-package magit-gptcommit
  :after magit
  :demand t
  :commands
  (magit-gptcommit-status-buffer-setup)
  :defines
  (my/gemini-llm-provider)
  :config
  (require 'my-llm)
  (setopt magit-gptcommit-llm-provider my/gemini-llm-provider)
  (magit-gptcommit-status-buffer-setup))

(use-package ellama
  :bind ("C-c o" . ellama)
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :commands ellama-session-mode-line-global-mode
  :defines my/gemini-llm-provider
  :config
  (require 'my-llm)
  (setopt ellama-auto-scroll t
          ellama-language "中文"
          ellama-sessions-directory (expand-file-name "ellama-sessions" my/emacs-cache-dir)
          ellama-provider (funcall my/gemini-llm-provider)
          ellama-define-word-prompt-template "定义 %s")
  (ellama-session-mode-line-global-mode))

(provide 'init-llm)

;;; init-llm.el ends here
