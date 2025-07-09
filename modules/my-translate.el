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


(use-package emacs
  :after
  (separedit)
  :bind
  (("C-c t" . my/translate-dwim))
  :config
  (require 'my-llm)
  (setq my/translate-llm-provider my/generic-llm-provider)

  (defvar my/translate-buffer-name "*my/translate*")
  (defvar my/translate-buffer nil)

  (defun my/translate-dwim ()
    "Translate dwim."
    (interactive)
    (let* ((parent-buffer (current-buffer))
           (block (cond
                   ((memq major-mode '(markdown-mode org-mode gfm-mode))
                    (list :beginning (point-min)
                          :end (point-max)
                          :major-mode major-mode))
                   (t (separedit--block-info))))
           (beg (plist-get block :beginning))
           (end (plist-get block :end))
           (block-major-mode (plist-get block :major-mode))
           (buffer (or (get-buffer my/translate-buffer-name)
                       (generate-new-buffer my/translate-buffer-name))))
      (with-current-buffer buffer
        (erase-buffer)
        (funcall (or block-major-mode 'fundamental-mode))
        (llm-chat-streaming-to-point
         my/translate-llm-provider
         (llm-make-chat-prompt
          (with-current-buffer parent-buffer
            (buffer-substring-no-properties beg end))
          :context
          "
指令: 翻译成中文
要求:
- 不要添加任何解释
- 忽略注释
"
          :temperature 0)
         buffer (point-max)
         (lambda ()))
        )
      (display-buffer buffer))))

(provide 'my-translate)

;;; my-translate.el ends here
