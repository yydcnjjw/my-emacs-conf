;;; my-hammy.el --- hammy -*- lexical-binding: t -*-

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

(require 'hammy)
(require 'alert)

(hammy-define "Waiting"
  :documentation "Single-use timer that prompts for name and duration."
  :complete-p (do (> cycles 0))
  :before
  (lambda (hammy)
    (hammy-reset hammy)
    (setf (hammy-intervals hammy)
          (ring-convert-sequence-to-ring
           (list (interval
                  :name "Waiting"
                  :duration (read-string "Duration: ")
                  :advance 'auto)))))
  :after
  (lambda (_)
    (message "Waiting is over!")
    (alert "Waiting is over!")))

(provide 'my-hammy)

;;; my-hammy.el ends here
