;;; my-smartparens.el --- smartparens -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (smartparens)


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

(use-package smartparens
  :defer t
  :init
  (progn
    ;; FIXME: https://github.com/Fuco1/smartparens/issues/1055
    (when my/emacs-28+
      (require 'comp)
      (add-to-list 'comp-bootstrap-black-list "smartparens")
      (add-to-list 'comp-deferred-compilation-black-list "smartparens"))

    (require 'smartparens-config)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)
    )
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              
              ("C-M-k" . sp-kill-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
              ("M-k"   . sp-backward-kill-sexp)

              ("C-M-[" . sp-backward-unwrap-sexp)
              ("C-M-]" . sp-unwrap-sexp)

              ("C-c ("  . sp-wrap-round)
              ("C-c ["  . sp-wrap-square)
              ("C-c {"  . sp-wrap-curly)))

(provide 'my-smartparens)

;;; my-smartparens.el ends here
