;;; my-dict.el --- dict -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: ()


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

(defun my/dict-buffer-view (process output)
  ""
  (with-current-buffer (get-buffer-create "*mdict*")
    (erase-buffer)
    (insert output)
    (org-mode)
    (org-show-all)
    (beginning-of-buffer)
    (display-buffer (current-buffer)
                    '((display-buffer-in-direction)
                      (direction . right)))
    )
  )

(defun my/query-dict (query)
  ""
  (interactive "sQuery word: ")
  (set-process-filter
   (start-process "mshell" nil "m" "de" query)
   #'my/dict-buffer-view
   )
  )

(defun my/query-word (query)
  ""
  (interactive "sQuery word: ")
  (set-process-filter
   (start-process "m" nil "m" "de" query)
   #'my/dict-buffer-view
   )
  )

(defun my/query-thesaures (query)
  ""
  (interactive "sQuery wthesaures: ")
  (set-process-filter
   (start-process "m" nil "m" "dt" query)
   #'my/dict-buffer-view
   )
  )

(bind-key "C-c d w" #'my/query-word)
(bind-key "C-c d t" #'my/query-thesaures)

(provide 'my-dict)

;;; my-dict.el ends here
