;;; my-logview.el --- logview -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (logview)

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

(use-package logview
  :custom
  ((logview-additional-level-mappings
   '(("PASOVAT" . ((error "error")
                   (warning "warning")
                   (information "info")
                   (debug "debug")
                   (trace "trace")
                   (aliases "pasovat")))))
   (logview-additional-timestamp-formats
    '(("PASOVAT" . ((java-pattern . "yyyy-MMM-dd HH:mm:ss.SSSSSS")))))
   (logview-additional-submodes
    ;; [][LEVEL][RX:IGNORED:[^]]+] MESSAGE
    '(("PASOVAT" . ((format . "[TIMESTAMP][THREAD][LEVEL]MESSAGE")
                    (levels . "PASOVAT")
                    (timestamp . ("PASOVAT"))))))))

(provide 'my-logview)

;;; my-logview.el ends here
