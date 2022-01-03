;;; my-lib.el --- my-lib -*- lexical-binding: t -*-

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

(require 'seq)



(defun my/executablesp (executables)
  "Find EXECUTABLES."
  (seq-filter #'(lambda (executable)
                  (cons executable (executable-find (symbol-name executable))))
              executables)
  )

(defun my/executablesp-log (executables)
  "Find EXECUTABLES."

  (let ((execlst (seq-filter #'(lambda (executable)
                                 (not (executable-find (symbol-name executable))))
                             executables))
        (msg "WARN: require system package:"))
    (dolist (exec execlst msg)
      (setq msg (concat msg " " (symbol-name exec))))
    (message msg)
    )
  )

(my/executablesp-log '(a abc))

(provide 'my-lib)

;;; my-lib.el ends here
