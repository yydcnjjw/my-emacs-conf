;;; my-anki.el --- my-anki -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (anki-editor)


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

(use-package anki-editor
  :straight (:host github :repo "orgtre/anki-editor")
  :commands anki-editor-mode
  :custom
  (anki-editor-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://rgb-24bit.github.io/org-html-theme-list/org/style/main.css\"/>")
  (anki-editor-org-tags-as-anki-tags nil)
  :init

  (cl-defun my/anki-editor--fetch (url
                                &rest settings
                                &key
                                (type "GET")
                                data success _error
                                (parser 'buffer-string)
                                &allow-other-keys)
    "Fetch URL using curl.
The api is borrowed from request.el."
    ;; This exists because request.el's sync mode calls curl asynchronously under
    ;; the hood, which doesn't work on some machines (like mine) where the process
    ;; sentinel never gets called. After some debugging of Emacs, it seems that in
    ;; 'process.c' the pselect syscall to the file descriptor of inotify used by
    ;; 'autorevert' always returns a nonzero value and causes 'status_notify' never
    ;; being called. To determine whether it's a bug in Emacs and make a patch
    ;; requires more digging.
    (let ((tempfile (make-temp-file "emacs-anki-editor"))
          (responsebuf (generate-new-buffer " *anki-editor-curl*")))
      (when data
        (with-temp-file tempfile
          (setq buffer-file-coding-system 'utf-8)
          (set-buffer-multibyte t)
          (insert data)))
      (unwind-protect
          (with-current-buffer responsebuf
            (apply #'call-process "curl" nil t nil (list
                                                    url
                                                    "--silent"
                                                    "-X" type
                                                    "--proxy"
                                                    (format "http://%s:%s" my/proxy-address my/proxy-port)
                                                    "--data-binary"
                                                    (concat "@" tempfile)))

            (goto-char (point-min))
            (when success
              (apply success (list :data (funcall parser)))))
        (kill-buffer responsebuf)
        (delete-file tempfile)
        )))

  (advice-add #'anki-editor--fetch :override #'my/anki-editor--fetch))

(provide 'my-anki)

;;; my-anki.el ends here
