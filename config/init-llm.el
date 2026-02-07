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
            "--noproxy" "127.0.0.1"
            ,@(when-let (proxy (getenv "ALL_PROXY")) (list "-x" proxy)))))

(use-package llm
  :straight (:type git :host github
                   :repo "yydcnjjw/llm"
                   :branch "main")
  :defer t
  :config
  (setopt llm-warn-on-nonfree nil))

(use-package emacs
  :ensure separedit
  :defer t
  :bind
  ("C-c o" . my/ai-menu)
  :config
  (require 'my-llm))

(use-package opencode
  :straight (opencode :type git :host codeberg :repo "sczi/opencode.el")
  :defer t
  :config
  (setq opencode-host "127.0.0.1"))

(provide 'init-llm)

;;; init-llm.el ends here
