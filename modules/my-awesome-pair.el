;;; my-awesome-pair.el --- awesome-pair -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (awesome-pair)


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

(use-package awesome-pair
  :straight (awesome-pair
             :host github
             :repo "manateelazycat/awesome-pair")
  :defer t
  :init
  (progn
    (dolist (hook (list
                   'c-mode-common-hook
                   'c-mode-hook
                   'c++-mode-hook
                   'java-mode-hook
                   'haskell-mode-hook
                   'emacs-lisp-mode-hook
                   'lisp-interaction-mode-hook
                   'lisp-mode-hook
                   'maxima-mode-hook
                   'ielm-mode-hook
                   'sh-mode-hook
                   'makefile-gmake-mode-hook
                   'php-mode-hook
                   'python-mode-hook
                   'js-mode-hook
                   'go-mode-hook
                   'qml-mode-hook
                   'jade-mode-hook
                   'css-mode-hook
                   'ruby-mode-hook
                   'coffee-mode-hook
                   'rustic-mode-hook
                   'qmake-mode-hook
                   'lua-mode-hook
                   'swift-mode-hook
                   'json-mode-hook
                   ))
      (add-hook hook #'(lambda ()
                         (require 'awesome-pair)
                         (awesome-pair-mode 1)
                         (show-paren-mode 1))
                )))
  :config
  (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
  (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
  (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
  (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
  (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
  (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
  (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

  (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
  (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

  (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)

  (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
  (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
  (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

  (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
  ;; (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
  (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
  (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
  (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

  (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
  (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
  (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)  )



(provide 'my-awesome-pair)

;;; my-awesome-pair.el ends here
