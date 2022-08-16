;;; my-yasnippet.el --- yasnippet -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (yasnippet)


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

(defvar my/yas-snippet-default-dir (expand-file-name "snippets" my/assets-dir))

(defun my/snippets-initialize ()
  "Load the snippets directory."
  (add-to-list 'yas-snippet-dirs 'my/yas-snippet-default-dir t)
  (yas-load-directory my/yas-snippet-default-dir t))

(use-package yasnippet
  :defer t
  :init
  (progn
    (eval-after-load 'yasnippet
      '(my/snippets-initialize)))
  :hook
  ((prog-mode . yas-minor-mode)
   (text-mode . yas-minor-mode))
  :bind
  ("C-c y" . #'yas-insert-snippet))

(use-package yasnippet-snippets
  :after (yasnippet))

(provide 'my-yasnippet)

;;; my-yasnippet.el ends here
