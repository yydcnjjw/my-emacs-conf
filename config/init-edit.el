;;; init-edit.el --- edit -*- lexical-binding: t -*-

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

(setq-default tab-width 4
              indent-tabs-mode nil)

(setopt require-final-newline t)

;; auto save
(use-package emacs
  :init
  (setopt auto-save-visited-interval 5
          auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" my/emacs-cache-dir))
  :hook
  ((prog-mode text-mode) . auto-save-visited-mode))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; bookmark
(setopt bookmark-default-file (expand-file-name "bookmarks" my/emacs-cache-dir))

;; tramp
(setopt tramp-persistency-file-name (expand-file-name "tramp" my/emacs-cache-dir))

;; transient
(use-package transient
  :config
  (setopt transient-levels-file (expand-file-name "transient/levels.el" my/emacs-cache-dir)
          transient-values-file (expand-file-name "transient/values.el" my/emacs-cache-dir)
          transient-history-file (expand-file-name "transient/history.el" my/emacs-cache-dir)))

;; recentf
(setopt recentf-save-file (expand-file-name "recentf" my/emacs-cache-dir))

;; store all backup and auto-save files in the tmp directory
(setopt backup-directory-alist
        `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

(delete-selection-mode t)

;; `which-key'
(use-package which-key
  :hook
  ((after-init . which-key-mode)))

;; ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

(use-package drag-stuff
  :functions
  (drag-stuff-define-keys)
  :config
  (drag-stuff-define-keys)
  :hook
  ((prog-mode . drag-stuff-mode)))

(use-package multiple-cursors
  :config
  (setopt mc/list-file (expand-file-name ".mc-lists.el" my/emacs-cache-dir))
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)))

(use-package edit-indirect
  :defer t)

(use-package window-numbering
  :commands
  (window-numbering-mode)
  :init
  (window-numbering-mode))

(use-package symbol-overlay
  :bind
  (("M-s s" . symbol-overlay-put)
   ("M-s M-s n" . symbol-overlay-switch-forward)
   ("M-s M-s p" . symbol-overlay-switch-backward)
   ("M-s M-s d" . symbol-overlay-remove-all)))

(use-package avy
  :bind
  ("C-c n" . avy-goto-char)
  :config
  (setopt avy-background t))

(use-package thing-edit
  :straight (:host github :repo "manateelazycat/thing-edit"
                   :branch "master")
  :defer t
  :init
  (autoload 'my/thing-edit-main-menu "my-thing-edit" nil t)
  (bind-key "M-s t" 'my/thing-edit-main-menu))

;; `expand-region'
(use-package expand-region
  :bind*
  ;; override `sp-copy-sexp'
  (("C-M-w" . er/expand-region)))

(use-package popup
  :defer t)

(use-package posframe
  :defer t)

(use-package rime
  :defer t
  :defines my/rime-user-data-dir
  :init
  (setopt default-input-method "rime")
  :config
  (require 'my-rime)
  (setopt rime-user-data-dir my/rime-user-data-dir
          rime-show-candidate 'posframe))

(use-package separedit
  :config
  (setq separedit-default-mode 'markdown-mode)
  (setopt separedit-continue-fill-column t
          separedit-remove-trailing-spaces-in-comment t)
  :bind
  ("C-c '" . separedit))

(use-package vundo
  :defer t)

(use-package indent-bars
  :config
  (setopt indent-bars-no-descend-lists 'skip
          indent-bars-treesit-support t
          indent-bars-treesit-ignore-blank-lines-types '("module")))

(use-package caser
  :bind
  (("M-C" . caser-camelcase-dwim)
   ("M-S" . caser-snakecase-dwim)
   ("M-D" . caser-dashcase-dwim)))

(use-package combobulate
  :defer t
  :defines combobulate-key-prefix
  :init
  (setq combobulate-key-prefix "C-c u")
  :hook ((prog-mode . combobulate-mode)))

(provide 'init-edit)

;;; init-edit.el ends here
