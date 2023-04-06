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

(defcustom my/en-font (cond
                      (my/linux-p
                       "Hack")
                      (my/windows-p
                       "Consolas")
                      (t "Hack"))
  "English font."
  :type 'string
  :group 'my)

(defcustom my/font-size 12
  "Font size."
  :type 'number
  :group 'my)

;; if gui do something in whatver type of emacs instance we are using
(defun my/apply-if-gui (&rest action)
  "Do specified ACTION if we're in a gui regardless of daemon or not."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (when (display-graphic-p frame)
                      (apply action))))
    (if (display-graphic-p)
        (apply action))))

(defun my/set-fontset-font (characters defaut-font &optional fallback-fonts)
  ""
  (set-fontset-font t characters defaut-font nil)
  (dolist (font fallback-fonts)
    (set-fontset-font t characters font nil 'append))
  (set-fontset-font t characters (font-spec :script characters) nil 'append))

(defun my/setup-font ()
  ""
  (add-to-list
   'default-frame-alist `(font . ,(format "%s-%d" my/en-font my/font-size)))

  (my/apply-if-gui
   (lambda ()
     (my/set-fontset-font 'han "霞鹜文楷等宽" '("Noto Sans Mono CJK"))
     (my/set-fontset-font 'emoji "Noto Emoji"))))

(my/setup-font)

(tool-bar-mode -1)
 
(menu-bar-mode -1)

(require 'scroll-bar)
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
