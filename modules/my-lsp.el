;;; my-lsp.el --- lsp -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (lsp-mode lsp-ui dap-mode)


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

(use-package emacs
  :custom
  (read-process-output-max (* 1024 1024)))

(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-mode
  :defer t
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :none)
  (lsp-auto-execute-action nil)
  (lsp-keep-workspace-alive nil)
  :init
  (defun my/lsp-completion-mode ()
    (if (featurep 'orderless)
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
              '(orderless)))
    (lsp-enable-which-key-integration)
    (lsp-inline-completion-company-integration-mode))

  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

  :hook
  ((lsp-completion-mode . my/lsp-completion-mode))

  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-cursor t))

(use-package lsp-ivy
  :defer t
  :commands lsp-ivy-workspace-symbol)

;; for debugger
;; (use-package dap-mode
;;   :defer t)

(provide 'my-lsp)

;;; my-lsp.el ends here
