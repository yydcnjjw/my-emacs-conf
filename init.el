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

(defvar my/root-dir (file-name-directory load-file-name)
  "The root dir of my Emacs.")
(setq user-emacs-directory my/root-dir)

(defvar my/core-dir (expand-file-name "core" my/root-dir)
  "Core dir.")

(defvar my/modules-dir (expand-file-name "modules" my/root-dir)
  "Modules dir.")

(defvar my/dy-modules-dir (expand-file-name "dy-modules" my/root-dir)
  "Dynamic modules dir.")

(defvar my/vendor-dir (expand-file-name "vendor" my/root-dir)
  "Vendor dir.")

(defvar my/assets-dir (expand-file-name "assets" my/root-dir)
  "Assets dir.")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; add directories to Emacs's `load-path'
(add-to-list 'load-path my/core-dir)
(add-to-list 'load-path my/modules-dir)
(add-to-list 'load-path my/vendor-dir)
(defun add-subdirs-to-load-path (dir)
  "Recursive add directory DIR to `load-path'."
  (mapcar
   (lambda (path) (add-to-list 'load-path path))
   (delete-dups (mapcar 'file-name-directory
                        (directory-files-recursively dir "\.el$")))))
(add-subdirs-to-load-path my/vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; load the core stuff
(require 'my-core)

;; modules
(require 'my-modules)

;;; init.el ends here
