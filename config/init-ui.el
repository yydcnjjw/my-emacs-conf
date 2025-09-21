;;; init-ui.el --- ui -*- lexical-binding: t -*-

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

(require 'my-frame)

(my/eval-if-graphic
 (lambda (frame)
   ;; setup font
   (let ((en-font-name "Hack")
         (zh-font-name "LXGW WenKai Mono")
         (font-size (if (my/is-screen-2k)
                        11
                      10)))
     (set-frame-font (format "%s-%d" en-font-name font-size))
     (my/set-fontset-font 'han zh-font-name nil frame)
     (my/set-fontset-font 'unicode zh-font-name nil nil)
     (my/set-fontset-font 'emoji "Noto Color Emoji" nil frame)

     (set-face-attribute 'fixed-pitch nil :family "Hack")
     (set-face-attribute 'variable-pitch nil :family "LXGW WenKai"))

   ;; setup frame position
   (my/center-frame frame)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(blink-cursor-mode -1)

(setq-default cursor-type 'bar
              ;; nice scrolling
              scroll-margin 0
              scroll-conservatively 100000
              scroll-preserve-screen-position 1)

;; disable startup screen
(setopt inhibit-startup-screen t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(use-package display-line-numbers
  :defer t
  :init
  (setopt display-line-numbers-width-start t)
  :hook
  ((prog-mode . display-line-numbers-mode)))

(use-package hl-line
  :defer t
  :hook
  (prog-mode . hl-line-mode))

(defun init-theme (frame)
  "Init theme for FRAME."
  (select-frame frame)
  (load-theme 'spacemacs-dark t nil))

(use-package spacemacs-theme
  :defer t
  :init
  
  (my/eval-if-graphic #'init-theme))

(use-package dashboard
  :functions
  (dashboard-setup-startup-hook)
  :init
  (dashboard-setup-startup-hook)
  :config
  (setopt dashboard-items '()))

(use-package doom-modeline
  :commands
  (doom-modeline-mode)
  :init
  (doom-modeline-mode))

(use-package hl-todo
  :defer t
  :hook
  ((prog-mode . hl-todo-mode))
  :init
  (setopt hl-todo-keyword-faces
          '(("TODO"  . "#28ABE3")
            ("FIXME" . "#DB3340")
            ("BUG"   . "#DB3340")
            )))

(use-package olivetti
  :defer t)

(provide 'init-ui)

;;; init-ui.el ends here
