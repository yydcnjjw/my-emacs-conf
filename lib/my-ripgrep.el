;;; my-ripgrep.el --- ripgrep -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: version
;; Package-Requires: (ripgrep)
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

(defun my/ripgrep-regexp-quote-globs (function regexp directory &optional arguments)
  "Call FUNCTION with shell-quoted glob options in ARGUMENTS.
`ripgrep-regexp' builds a shell command string, so glob options must be
quoted before the user's shell parses them."
  (funcall function regexp directory
           (mapcar (lambda (argument)
                     (if (and (stringp argument)
                              (string-prefix-p "--glob=" argument))
                         (shell-quote-argument argument)
                       argument))
                   arguments)))

(provide 'my-ripgrep)

;;; my-ripgrep.el ends here
