;;; my-editor.el --- editor -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (which-key)


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

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq require-final-newline t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq blink-matching-paren nil)

;; auto save
(use-package emacs
  :custom
  (auto-save-visited-interval 5)
  :hook
  ((prog-mode text-mode) . #'auto-save-visited-mode))

;; local variable
(setq enable-local-variables :all
      enable-local-eval t)

;; `which-key'
(use-package which-key
  :defer t
  :hook
  ((after-init . which-key-mode)))

;; `expand-region'
(use-package expand-region
  :defer t
  :bind
  (("C-M-w" . er/expand-region)))

;; TODO: comment tag

;; ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; (use-package pangu-spacing
;;   :custom
;;   (pangu-spacing-real-insert-separtor t)
;;   :config
;;   (global-pangu-spacing-mode 1))

(use-package drag-stuff
  :config
  (drag-stuff-define-keys)
  :hook
  ((prog-mode) . drag-stuff-mode))

(use-package multiple-cursors
  :defer t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)))

(use-package edit-indirect
  :defer t)

(use-package window-numbering
  :init
  (window-numbering-mode))

(use-package symbol-overlay
  :bind
  (("M-s s" . symbol-overlay-put)
   ("M-s M-s n" . symbol-overlay-switch-forward)
   ("M-s M-s p" . symbol-overlay-switch-backward)
   ("M-s M-s d" . symbol-overlay-remove-all)))


;; https://www.lukas-barth.net/blog/emacs-wsl-copy-clipboard/
(if my/wsl-p
    (setq select-active-regions nil))

(provide 'my-editor)

;;; my-editor.el ends here
