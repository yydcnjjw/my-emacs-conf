;;; my-d2.el --- d2 -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: version
;; Package-Requires: (d2-mode)
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

(require 'd2-mode)

(defun my/org-babel-execute:d2 (body params)
  "Execute command with BODY and PARAMS from src block."
  (let* ((out-file (or (cdr (assoc :file params))
                       (error "D2 requires a \":file\" header argument")))
         (temp-file (org-babel-temp-file "d2-"))
         (cmd (mapconcat #'shell-quote-argument
                         (append (list d2-location
                                       temp-file
                                       (org-babel-process-file-name out-file))
                                 d2-flags)
                         " ")))
    (with-temp-file temp-file (insert body))
    (message (format "sh -c \"o=\\$(%s 2>&1) || echo \\$o >&2\"" cmd))
    (org-babel-eval (format "sh -c \"o=\\$(%s 2>&1) || echo \\$o >&2\"" cmd) "")
    nil))

(provide 'my-d2)

;;; my-d2.el ends here
