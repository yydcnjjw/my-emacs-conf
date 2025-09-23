;;; my-frame.el --- frame -*- lexical-binding: t -*-

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

(defmacro my/eval-if-graphic (action &optional depth)
  "Do specified ACTION with DEPTH.

if we're in a graphic regardless of daemon or not."
  (if (daemonp)
      `(add-hook 'after-make-frame-functions
                 #'(lambda (frame)
                     (when (display-graphic-p frame)
                       (funcall ,action frame))) ,depth)
    `(when (display-graphic-p)
       (funcall ,action (selected-frame)))))

(defun my/is-screen-2k (&optional frame)
  "Is screen 2k with FRAME."
  (> (display-pixel-width frame) 1920))

(defun my/center-frame (frame)
  "Center FRAME."
  (let* ((screen-width (display-pixel-width frame))
         (screen-height (display-pixel-height frame))
         (frame-width (frame-pixel-width frame))
         (frame-height (frame-pixel-height frame))
         (left-pos (max 0 (/ (- screen-width frame-width) 2)))
         (top-pos (max 0 (/ (- screen-height frame-height) 2))))
    (message "screen width: %d screen height: %d" screen-width screen-height)
    (message "frame width: %d frame height: %d" screen-width screen-height)
    (message "left: %d top: %d" left-pos top-pos)
    (set-frame-position frame left-pos top-pos)))

(defun my/set-fontset-font (characters defaut-font &optional fallback-fonts frame)
  "Set fontset font with CHARACTERS DEFAUT-FONT &optional FALLBACK-FONTS FRAME."
  (set-fontset-font t characters defaut-font frame)
  (dolist (font fallback-fonts)
    (set-fontset-font t characters font frame 'append))
  (set-fontset-font t characters (font-spec :script characters) frame 'append))

(provide 'my-frame)

;;; my-frame.el ends here
