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

(defun add-hooks-listify (object)
  "If OBJECT is a list and not a function, return it, else wrap it in a list."
  (if (and (listp object)
           (not (functionp object)))
      object
    (list object)))

(defun add-hooks-normalize-hook (hook)
  "If HOOK is a symbol, ensure `-hook' is appended, else return HOOK itself."
  (if (and (symbolp hook)
           (not (string-match "-hook$" (symbol-name hook))))
      (intern (concat (symbol-name hook) "-hook"))
    hook))

(defun add-hooks-pair (hooks functions)
  "Call `add-hook' for each combined pair of items in HOOKS and FUNCTIONS.
HOOKS can be a symbol or a list of symbols representing hook
variables (the `-hook' suffix is implied).  FUNCTIONS can be a
symbol, a lambda, or a list of either representing hook
functions.  If lists are used, a function can be added to
multiple hooks and/or multiple functions can be added to a hook.
Example:
  ELISP> (add-hooks-pair '(css-mode sgml-mode) 'emmet-mode)
  nil
  ELISP> css-mode-hook
  (emmet-mode)
  ELISP> sgml-mode-hook
  (emmet-mode)"
  (dolist (hook (mapcar 'add-hooks-normalize-hook (add-hooks-listify hooks)))
    (dolist (function (add-hooks-listify functions))
      (add-hook hook function))))

(defun add-hooks (pairs)
  "Call `add-hooks-pair' on each cons pair in PAIRS.
Each pair has a `car' for setting hooks and a `cdr' for setting
functions to add to those hooks.  Pair values are passed to the
HOOKS and FUNCTIONS arguments of `add-hooks-pair', respectively.
Usage:
  (add-hooks ((HOOKS . FUNCTIONS)...))
Example:
  ELISP> (add-hooks '(((css-mode sgml-mode) . emmet-mode)))
  nil
  ELISP> css-mode-hook
  (emmet-mode)
  ELISP> sgml-mode-hook
  (emmet-mode)"
  (dolist (pair pairs)
    (add-hooks-pair (car pair) (cdr pair))))

(provide 'my-lib)

;;; my-lib.el ends here
