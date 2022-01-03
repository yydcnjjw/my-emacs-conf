;;; my-eaf.el --- eaf -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (eaf)


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

;; (defun my/eaf-org ()
;;   (require 'eaf-org)
;;   ;; use `emacs-application-framework' to open PDF file: link
;;   (defun eaf-org-open-file (file &optional link)
;;     "An wrapper function on `eaf-open'."
;;     (eaf-open file))
;;   (add-to-list 'org-file-apps '("\\.pdf\\'" . eaf-org-open-file)))
    

(use-package eaf
  :straight (:host github
                   :repo  "manateelazycat/emacs-application-framework"
                   :files ("*"))
  ;; :defer t
  :if window-system
  
  ;; :hook
  ;; ((org-mode . my/eaf-org))
  ;; :custom
  ;; (eaf-browser-continue-where-left-off t)
  ;; :config
  ;; (setq eaf-browser-enable-adblocker t)
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  )

(use-package eaf-browser
  :after eaf
  :straight (:host github
                   :repo  "emacs-eaf/eaf-browser"
                   :files ("*")))

(use-package eaf-markdown-previewer
  :after eaf-browser
  :straight (:host github
                   :repo  "emacs-eaf/eaf-markdown-previewer"
                   :files ("*")))

(provide 'my-eaf)

;;; my-eaf.el ends here
