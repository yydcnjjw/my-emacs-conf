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
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq blink-matching-paren nil)

;; auto save
(setq auto-save-visited-interval 1)
(add-hook 'prog-mode-hook #'auto-save-visited-mode)
(add-hook 'text-mode-hook #'auto-save-visited-mode)

;; local variable
(setq enable-local-variables :all
      enable-local-eval t)

;; `which-key'
(use-package which-key
  :config
  (which-key-mode))


;; `expand-region'
(use-package expand-region
  :bind
  (("C-M-w" . er/expand-region)))

;; comment


(provide 'my-editor)

;;; my-editor.el ends here
