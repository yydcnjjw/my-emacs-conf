;;; my-translate.el --- translate -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (insert-translated-name go-translate)


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

(use-package insert-translated-name
  :straight (:host github
                   :repo "yydcnjjw/insert-translated-name"
                   :branch "old"
                   ;; :files ("*.el" "*.ts")
                   )
  :bind
  (("C-c C" . insert-translated-name-insert))
  :config
  (setq insert-translated-name-translate-engine "youdao"))

;; (use-package insert-translated-name
;;   :straight (:host github
;;                    :repo "manateelazycat/insert-translated-name"
;;                    :branch "master"
;;                    :files ("*.el" "*.ts")
;;                    )
;;   :init
;;   (use-package deno-bridge
;;     :straight (:host github
;;                      :repo "manateelazycat/deno-bridge"
;;                      :branch "master"
;;                      :files ("*.el")
;;                      )
;;     )
;;   (defun my/start-insert-translated-name ()
;;     ""
;;     (interactive)
;;     (unless (member "insert-translated-name" deno-bridge-app-list)
;;       (deno-bridge-start "insert-translated-name" deno-translator-ts-path)))
;;   :bind
;;   (("C-c C" . insert-translated-name-insert)))

(use-package go-translate
  :defer t
  :bind
  (("C-c t" . gts-do-translate))
  :custom
  (gts-translate-list '(("en" "zh")))
  (gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-bing-engine)
                        (gts-google-engine))
         :render (gts-buffer-render))))

(provide 'my-translate)

;;; my-translate.el ends here
