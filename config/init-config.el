;;; init-config.el --- config -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: version
;; Package-Requires: ()
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

(let (;; 加载的时候临时增大 `gc-cons-threshold' 以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6))

  (my/require-modules
   init-package
   ;; init-bench
   init-os
   init-ui
   init-generic
   init-edit
   init-project
   init-vc
   init-completing-read
   init-completion
   init-language-completion
   init-prog-language
   init-syntax-check
   init-org
   init-im
   init-rss
   init-llm
   init-email
   init-term
   )
  )


(provide 'init-config)

;;; init-config.el ends here
