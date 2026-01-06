;;; init-chat.el --- chat -*- lexical-binding: t -*-

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

(use-package telega
  :commands (telega)
  :defer t
  :bind-keymap
  ("C-c r" . telega-prefix-map)
  :commands
  (telega-mode-line-mode telega-transient-mode)
  :init
  (defun my/telega-load ()
    (telega-mode-line-mode)
    (require 'telega-transient)
    (telega-transient-mode))
  :config
  (setopt telega-server-libs-prefix "/usr"
          telega-proxies
          (list
           '(:server "127.0.0.1" :port 9888 :enable t
                     :type (:@type "proxyTypeSocks5"))))
  :hook
  ((telega-load-hook . my/telega-load)))

(use-package ement
  :straight (:host github :repo "alphapapa/ement.el")
  :defer t)

(provide 'init-chat)

;;; init-chat.el ends here
