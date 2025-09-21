;;; init-prog-language.el --- prog-language -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: version
;; Package-Requires: (dependencies)
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

(require 'my-loading)
(require 'my-path)

(use-package emacs
  :commands lsp
  :functions my/lsp-register-major-mode
  :init
  (defun my/lsp-register-major-mode (&rest modes)
    "Lsp register major MODE."
    (dolist (mode modes)
      (add-hook 'hack-local-variables-hook
	      #'(lambda ()
		  (when (derived-mode-p mode) (lsp)))))
    ))

(my/require-modules
 init-rust
 init-python
 init-latex
 ;; init-web
 )

;; C/C++
(use-package emacs
  :init
  (my/lsp-register-major-mode 'c-mode 'c++-mode 'objc-mode)
  (setopt lsp-clients-clangd-args
   '("--header-insertion-decorators=0"
     "--header-insertion=never"
     "--clang-tidy"
     "--clang-tidy-checks=performance-*,bugprone-*"
     "--background-index"
     "--all-scopes-completion"
     "--pch-storage=memory"
     "-j=4"
     "--malloc-trim"))
  :mode
  (("\\.ipp\\'" . c++-mode)
   ("\\.mm\\'" . objc-mode)
   ("\\.cmake\\'" . cmake-ts-mode)))

(use-package go-mode
  :defer t
  :init
  (my/lsp-register-major-mode 'go-mode)
  (setopt go-ts-mode-indent-offset 4))

(use-package php-mode
  :defer t
  :init
  (my/lsp-register-major-mode 'php-mode))

(use-package just-mode
  :defer t)

(use-package lua-mode
  :defer t
  :init
  (my/lsp-register-major-mode 'lua-mode))

(use-package json-mode
  :defer t)

(use-package yaml-pro
  :defer t
  :hook (yaml-mode . yaml-pro-ts-mode))

(use-package markdown-mode
  :defer t
  :ensure valign
  :mode ("\\.md\\'" . gfm-mode)
  :init
  (setopt markdown-command "pandoc")
  :hook (gfm-mode . valign-mode))

(use-package protobuf-mode
  :defer t
  :straight (:host github
                   :repo "emacsmirror/protobuf-mode")
  :mode (("\\.proto\\'" . protobuf-mode)))

;; TODO
;; (use-package d2-mode
;;   :defer t
;;   :ensure-system-package d2
;;   ;; :config
;;   ;; (my/push-load-org-babel-language 'd2)
;;   )

;; TODO
;; (use-package plantuml-mode
;;   :custom
;;   ((plantuml-default-exec-mode 'executable))
;;   :defer t
;;   :after org
;;   :config
;;   (setq org-plantuml-exec-mode 'plantuml)
;;   (my/push-load-org-babel-language 'plantuml))

(use-package logview
  :defer t
  :init
  (setopt
   logview-cache-filename (expand-file-name "logview-cache.extmap" my/emacs-cache-dir)
   logview-additional-level-mappings '(("PASOVAT" . ((error "error")
						     (warning "warning")
						     (information "info")
						     (debug "debug")
						     (trace "trace")
						     (aliases "pasovat"))))
   logview-additional-timestamp-formats
   '(("PASOVAT" . ((java-pattern . "yyyy-MMM-dd HH:mm:ss.SSSSSS"))))
   logview-additional-submodes
   ;; [][LEVEL][RX:IGNORED:[^]]+] MESSAGE
   '(("PASOVAT" . ((format . "[TIMESTAMP][THREAD][LEVEL]MESSAGE")
		   (levels . "PASOVAT")
		   (timestamp . ("PASOVAT")))))
   ))

(provide 'init-prog-language)

;;; init-prog-language.el ends here
