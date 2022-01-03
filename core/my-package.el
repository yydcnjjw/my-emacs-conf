;;; my-package.el --- package -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (use-package use-package-ensure-system-package auto-package-update gnu-elpa-keyring-update)


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

;; (require 'package)

;; (setq package-archives
;;       '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;         ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
;;         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
;;         ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

;; (package-initialize)
;; (defun require-package (package)
;;   "Install PACKAGE unless already installed."
;;   (or (package-installed-p package)
;;       (if (assoc package package-archive-contents)
;;           (package-install package)
;;         (progn
;;           (package-refresh-contents)
;;           (require-package package)))))

;; (require-package 'use-package)

;; (require 'use-package)
;; (setq ;; use-package-verbose t
;;       use-package-always-ensure t)

;; (use-package use-package-ensure-system-package)

;; (use-package quelpa
;;   :config
;;   (setq quelpa-update-melpa-p nil
;;         quelpa-checkout-melpa-p nil
;;         quelpa-upgrade-interval 7
;;         ))

;; (use-package quelpa-use-package)

;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t
;;         auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

;; (use-package gnu-elpa-keyring-update)

(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package use-package-ensure-system-package)

(provide 'my-package)

;;; my-package.el ends here
