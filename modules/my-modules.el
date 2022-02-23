;;; my-modules.el --- modules -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw


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

(defcustom my/required-module
  '(dashboard
    doom-modeline
    ;; smartparens
    awesome-pair
    ;; awesome-tray
    projectile
    vc
    company
    lsp
    ivy
    avy
    yasnippet
    org
    c-cpp
    lisp
    go
    ts
    rust
    python
    web
    pest
    json
    spell
    latex
    ;; flutter
    xclip
    im
    hl-todo
    ;; eaf
    ;; leetcode
    plantuml
    translate
    atomic-chrome
    vterm
    elfeed
    pyim
    markdown
    ;; rime
    yaml
    ;; embark
    ;; grammarly
    mu4e
    ;; wl
    alert
    )
  "")

(dolist (module my/required-module)
  (require (intern (concat "my-" (symbol-name module)))))

(provide 'my-modules)

;;; my-modules.el ends here
