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
                    (when (derived-mode-p mode) (lsp)))))))

(use-package emacs
  :after treesit
  :functions (my/treesit-setup my/treesit-register)
  :init
  (require 'my-treesit)
  (my/treesit-setup))

(my/require-modules
 init-rust
 init-python
 init-latex
 ;; init-web
 )

;; C/C++
(use-package emacs
  :init
  (my/lsp-register-major-mode 'c-mode 'c++-mode 'objc-mode 'cmake-ts-mode)
  (setopt lsp-clients-clangd-args
          '("--header-insertion-decorators=0"
            "--header-insertion=never"
            "--clang-tidy"
            "--clang-tidy-checks=performance-*,bugprone-*"
            "--background-index"
            "--all-scopes-completion"
            "--pch-storage=memory"))
  :mode
  (("\\.ipp\\'" . c++-mode)
   ("\\.mm\\'" . objc-mode)))

(use-package emacs
  :after treesit
  :mode (("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c-or-c++-mode . c-or-c++-ts-mode))
  (my/treesit-register
   '(:lang cpp
           :source ("https://github.com/tree-sitter/tree-sitter-cpp")
           :mode (c++-mode c++-ts-mode))
   '(:lang c
           :source ("https://github.com/tree-sitter/tree-sitter-c")
           :mode (c-mode c-ts-mode))
   '(:lang cmake
           :source ("https://github.com/uyha/tree-sitter-cmake")
           :mode (cmake-ts-mode))))
;; go
(use-package emacs
  :mode (("\\.go\\'" . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode))
  :init
  (my/lsp-register-major-mode 'go-ts-mode 'go-mod-ts-mode)
  (setopt go-ts-mode-indent-offset 4))

(use-package emacs
  :after treesit
  :init
  (my/treesit-register
   '(:lang go
           :source ("https://github.com/tree-sitter/tree-sitter-go")
           :mode (go-ts-mode))
   '(:lang gomod
           :source ("https://github.com/camdencheek/tree-sitter-go-mod")
           :mode (go-mod-ts-mode))))

(use-package php-mode
  :defer t
  :init
  (my/lsp-register-major-mode 'php-mode)
  :config
  (setopt lsp-intelephense-storage-path (expand-file-name "lsp-php-cache" my/emacs-cache-dir)
          lsp-intelephense-global-storage-path (expand-file-name "php-intelephense" my/emacs-cache-dir)))

(use-package just-mode
  :defer t)

(use-package lua-mode
  :defer t
  :init
  (my/lsp-register-major-mode 'lua-mode))

(use-package emacs
  :after treesit
  :mode (("\\.json\\'" . json-ts-mode)
         ("\\.jsonc\\'" . json-ts-mode))
  :init
  (my/treesit-register
   '(:lang json
           :source ("https://github.com/tree-sitter/tree-sitter-json")
           :mode (json-ts-mode)))
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode)))

;; YAML
(use-package yaml-pro
  :after treesit
  :functions (yaml-pro-ts-mode indent-bars-mode)
  :hook (yaml-ts-mode . my/yaml-ts-mode)
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :init
  (defun my/yaml-ts-mode ()
    (yaml-pro-ts-mode)
    (when (boundp 'indent-bars-mode)
      (indent-bars-mode)))
  (my/treesit-register
   '(:lang yaml
           :source ("https://github.com/tree-sitter-grammars/tree-sitter-yaml")
           :mode (yaml-ts-mode)))
  (my/lsp-register-major-mode 'yaml-ts-mode)
  :config
  (setopt yaml-pro-indent 4))

(use-package markdown-mode
  :defer t
  :ensure valign
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setopt markdown-command "pandoc")
  :hook (gfm-mode . valign-mode))

(use-package protobuf-mode
  :defer t
  :straight (:host github
                   :repo "emacsmirror/protobuf-mode")
  :mode (("\\.proto\\'" . protobuf-mode)))

(use-package d2-mode
  :defer t
  :ensure-system-package d2
  :defines org-babel-load-languages
  :config
  (require 'my-d2)
  (setopt d2-flags '("--theme=200" "--pad=0" "--center" "--sketch"))
  (advice-add 'org-babel-execute:d2 :override 'my/org-babel-execute:d2))

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
  :config
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
