;;; my-corfu.el --- corfu -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (corfu)


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

(use-package popon
  :straight (popon
             :type git
             :repo "https://codeberg.org/akib/emacs-popon.git"))

(use-package corfu-terminal
  :straight (corfu-terminal
             :type git
             :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(use-package corfu-doc-terminal
  :straight (corfu-doc-terminal
             :type git
             :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))

(use-package corfu-doc
  :ensure t
  :hook
  (corfu-mode . corfu-doc-mode))

(use-package corfu
  :init
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  
  (global-corfu-mode)
  (corfu-terminal-mode +1)
  (corfu-doc-terminal-mode +1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-on-exact-match nil)
  ;; :hook
  ;; (minibuffer-setup . #'corfu-enable-always-in-minibuffer)
  )

(use-package cape
  :bind (("C-c f p" . completion-at-point) ;; capf
         ("C-c f t" . complete-tag)        ;; etags
         ("C-c f d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c f h" . cape-history)
         ("C-c f f" . cape-file)
         ("C-c f k" . cape-keyword)
         ("C-c f s" . cape-symbol)
         ("C-c f a" . cape-abbrev)
         ("C-c f i" . cape-ispell)
         ("C-c f l" . cape-line)
         ("C-c f w" . cape-dict)
         ("C-c f \\" . cape-tex)
         ("C-c f _" . cape-tex)
         ("C-c f ^" . cape-tex)
         ("C-c f &" . cape-sgml)
         ("C-c f r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
)

(provide 'my-corfu)

;;; my-corfu.el ends here
