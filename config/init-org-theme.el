;;; init-org-theme.el --- org-theme -*- lexical-binding: t -*-

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

(use-package org
  :defer t
  :init
  (setopt org-adapt-indentation t
	  org-hide-leading-stars t
	  org-pretty-entities t
	  org-ellipsis " ..."
	  org-startup-folded 'nofold
	  org-hide-drawer-startup t)

  (setopt org-src-fontify-natively t
	  org-src-tab-acts-natively t
	  org-edit-src-content-indentation 0)

  (setopt org-auto-align-tags t
	  org-tags-column -80
	  org-fold-catch-invisible-edits 'show-and-error
	  org-special-ctrl-a/e t
	  org-insert-heading-respect-content t)

  (setopt org-priority-lowest ?F
	  org-priority-default ?E)

  (setq org-priority-faces
	'((65 . "#BF616A")
          (66 . "#EBCB8B")
          (67 . "#B48EAD")
          (68 . "#81A1C1")
          (69 . "#5E81AC")
          (70 . "#4C566A")))
  :config
  (require 'my-org-theme)

  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.35)
                  (org-level-3 . 1.3)
                  (org-level-4 . 1.25)
                  (org-level-5 . 1.2)
                  (org-level-6 . 1.15)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :height (cdr face)))

  (require 'org-indent)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch :height 0.85)
  (set-face-attribute 'org-block-begin-line nil :inherit '(org-meta-line))
  (set-face-attribute 'org-block-end-line nil :inherit '(org-meta-line))
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch) :height 0.85)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch) :height 0.85)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch) :height 0.85)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-keyword-face fixed-pitch) :height 0.85)
  (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch :height 0.85)
  (set-face-attribute 'org-drawer nil :inherit 'fixed-pitch :height 0.85)
  (set-face-attribute 'org-date nil :inherit 'fixed-pitch :height 0.85)
  (set-face-attribute 'org-tag nil :inherit 'fixed-pitch :height 0.85)
  (set-face-attribute 'org-document-info-keyword nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit '(bold fixed-pitch))

  (defun my/org-theme-init-mode ()
    (when (my/is-screen-2k)
      (face-remap-add-relative 'default :height 1.25))
    
    (variable-pitch-mode)
    (visual-line-mode)

    (setq-local olivetti-body-width 120)
    
    (olivetti-mode)

    (org-indent-mode)
    (my/setup-prettify-symbols))
  
  :hook
  ((org-mode . my/org-theme-init-mode))
  :commands
  (olivetti-mode
   my/setup-prettify-symbols
   org-indent-mode)
  )

(use-package org-appear
  :commands
  (org-appear-mode)
  :hook
  (org-mode . org-appear-mode)
  :config
  (setopt org-hide-emphasis-markers t)
  (setopt org-appear-autoemphasis t
          org-appear-autolinks t
	  org-appear-autosubmarkers t))


(use-package org-superstar
  :config
  (setopt org-superstar-leading-bullet " "
	  org-superstar-headline-bullets-list '("◉" "○" "⚬" "◈" "◇")
	  org-superstar-special-todo-items t
	  org-superstar-todo-bullet-alist '(("TODO"  . 9744)
                                            ("WAIT"  . 9744)
                                            ("READ"  . 9744)
                                            ("PROG"  . 9744)
					    ("DONE"  . 9745)))
  :hook (org-mode . org-superstar-mode))

(use-package svg-tag-mode
  :functions
  (my/setup-org-svg-tags)
  :config
  (require 'my-org-theme-svg)
  (my/setup-org-svg-tags)
  :hook
  ((org-mode . svg-tag-mode)))

;; Latex
(use-package org-fragtog
  :config
  (plist-put org-format-latex-options :scale 1.35)
  :hook (org-mode . org-fragtog-mode))

;; table align
(use-package valign
  :defer t
  :hook (org-mode . valign-mode))

(provide 'init-org-theme)

;;; init-org-theme.el ends here
