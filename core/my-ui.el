;;; my-ui.el --- UI optimizations and tweaks -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (spacemacs-theme which-key)


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

;; Setting English Font

(require 'my-const)

(defconst my/en-font (cond
                      (my/linux-p
                       "Hack")
                      (my/windows-p
                       "Consolas")
                      (t "Hack")))

(defconst my/zh-font (cond
                      (my/linux-p
                       "Noto Sans CJK SC")
                      (my/windows-p
                       "Consolas")
                      (t "Noto Sans CJK SC")))

(set-face-attribute 'default nil
                    :font (format "%s:pixelsize=%d"
                                  my/en-font
                                  16))

;; Chinese Font
(when window-system
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec
                       :family my/zh-font
                       ))))

(tool-bar-mode -1)
 
(menu-bar-mode -1)

(scroll-bar-mode -1)

(blink-cursor-mode -1)

(setq-default cursor-type 'bar)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; show line number
(use-package display-line-numbers
  :custom
  (display-line-numbers-width-start t)
  :hook
  ((prog-mode . display-line-numbers-mode)))

;; hl line mode
(global-hl-line-mode)

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

(provide 'my-ui)

;;; my-ui.el ends here
