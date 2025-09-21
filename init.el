;;; init.el --- configuration entry point -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: 0.0.1
;; Homepage: yydcnjjw
;; Keywords: yydcnjjw


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

;; Always load newest byte code
(setq load-prefer-newer t)

(defconst my/root-dir (file-name-directory load-file-name)
  "The root dir of my Emacs.")
(setq user-emacs-directory my/root-dir)

(defconst my/lib-dir (expand-file-name "lib" my/root-dir)
  "Lib directory.")

(defconst my/config-dir (expand-file-name "config" my/root-dir)
  "Config directory.")

(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; add directories to Emacs's `load-path'
(add-to-list 'load-path my/lib-dir)
(add-to-list 'load-path my/config-dir)

;; load the config
(require 'init-config)

;;; init.el ends here
