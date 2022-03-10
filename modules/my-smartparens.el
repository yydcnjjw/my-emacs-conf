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
  :config
  (require 'smartparens-config)

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

  (sp-with-modes 'org-mode
    (sp-local-pair "=" "=" :wrap "C-="))

  (sp-with-modes 'textile-mode
    (sp-local-pair "*" "*")
    (sp-local-pair "_" "_")
    (sp-local-pair "@" "@"))

  (sp-with-modes 'web-mode
    (sp-local-pair "{{#if" "{{/if")
    (sp-local-pair "{{#unless" "{{/unless"))

  ;; tex-mode latex-mode
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-tag "i" "\"<" "\">"))

  ;; lisp modes
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil
                   :wrap "C-("
                   :pre-handlers '(my/add-space-before-sexp-insertion)
                   :post-handlers '(my/add-space-after-sexp-insertion)))

  (defun my/add-space-after-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (forward-char (sp-get-pair id :cl-l))
        (when (or (eq (char-syntax (following-char)) ?w)
                  (looking-at (sp--get-opening-regexp)))
          (insert " ")))))

  (defun my/add-space-before-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (backward-char (length id))
        (when (or (eq (char-syntax (preceding-char)) ?w)
                  (and (looking-back (sp--get-closing-regexp))
                       (not (eq (char-syntax (preceding-char)) ?'))))
          (insert " ")))))

  ;; C++
  (sp-with-modes '(malabar-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                      ("* ||\n[i]" "RET")))

  ;; js/ts
  (sp-with-modes '(js2-mode typescript-mode)
    (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                               ("* ||\n[i]" "RET"))))

  :hook
  ((prog-mode . show-smartparens-mode)
   (prog-mode . smartparens-mode))
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)

              ("C-M-d" . sp-down-sexp)
              ("C-M-a" . sp-backward-down-sexp)
              ("C-S-d" . sp-beginning-of-sexp)
              ("C-S-a" . sp-end-of-sexp)
              
              ("C-M-e" . sp-up-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-t" . sp-transpose-sexp)

              ("C-M-n" . sp-forward-hybrid-sexp)
              ("C-M-p" . sp-backward-hybrid-sexp)
              
              ("C-M-k" . sp-kill-sexp)
              ;; ("C-M-w" . sp-copy-sexp)

              ("M-<delete>" . sp-unwrap-sexp)
              ("M-<backspace>" . sp-backward-unwrap-sexp)

              ("C-<right>" . sp-forward-slurp-sexp)
              ("C-<left>" . sp-forward-barf-sexp)
              ("C-M-<left>" . sp-backward-slurp-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)

              ("M-D" . sp-splice-sexp)
              ("C-M-<delete>" . sp-splice-sexp-killing-forward)
              ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
              ("C-S-<backspace>" . sp-splice-sexp-killing-around)

              ("C-]" . sp-select-next-thing-exchange)
              ("C-<left_bracket>" . sp-select-previous-thing)
              ("C-M-]" . sp-select-next-thing)

              ("M-F" . sp-forward-symbol)
              ("M-B" . sp-backward-symbol)

              ;; ("C-\"" . sp-change-inner)
              ("M-i" . sp-change-enclosing)))

(provide 'my-smartparens)

;;; my-smartparens.el ends here
