;;; my-org-theme-svg.el --- org-theme-svg -*- lexical-binding: t -*-

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

(require 'svg-lib)

(defun svg-progress-percent (value)
  "SVG progress percent based on VALUE."
  (svg-image (svg-lib-concat
	      (svg-lib-progress-bar (/ (string-to-number value) 100.0)
				    nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
	      (svg-lib-tag (concat value "%")
			   nil :stroke 0 :margin 0))
	     :ascent 'center)
  )

(defun svg-progress-count (value)
  "SVG progress count from VALUE."
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
    (svg-image (svg-lib-concat
		(svg-lib-progress-bar (/ count total) nil
				      :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		(svg-lib-tag value nil
			     :stroke 0 :margin 0))
	       :ascent 'center))
  )

(defun my/setup-org-svg-tags ()
  "Setup org svg tags."
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
  (setopt svg-tag-tags
	  `(
            ;; Task priority
            ("\\[#[A-Z]\\]" . ( (lambda (tag)
				  (svg-tag-make tag :face 'org-priority
						:beg 2 :end -1 :margin 0))))

            ;; Progress
            ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
						(svg-progress-percent (substring tag 1 -2)))))
            ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
					      (svg-progress-count (substring tag 1 -1)))))

            ;; Citation of the form [cite:@Knuth:1984]
            ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                              (svg-tag-make tag
                                                            :inverse t
                                                            :beg 7 :end -1
                                                            :crop-right t))))
            ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                       (svg-tag-make tag
								     :end -1
								     :crop-left t))))


            ;; Active date (with or without day name, with or without time)
            ;; (,(format "\\(<%s>\\)" date-re) .
            ;;  ((lambda (tag)
	    ;; 	(svg-tag-make tag :beg 1 :end -1 :margin 0))))
            ;; (,(format "\\(<%s \\)%s>" date-re day-time-re) .
            ;;  ((lambda (tag)
	    ;; 	(svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
            ;; (,(format "<%s \\(%s>\\)" date-re day-time-re) .
            ;;  ((lambda (tag)
	    ;; 	(svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

            ;; ;; Inactive date  (with or without day name, with or without time)
            ;; (,(format "\\(\\[%s\\]\\)" date-re) .
            ;;  ((lambda (tag)
	    ;; 	(svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
            ;; (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
            ;;  ((lambda (tag)
	    ;; 	(svg-tag-make tag :beg 1 :inverse nil
	    ;; 		      :crop-right t :margin 0 :face 'org-date))))
            ;; (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
            ;;  ((lambda (tag)
	    ;; 	(svg-tag-make tag :end -1 :inverse t
	    ;; 		      :crop-left t :margin 0 :face 'org-date))))
	    )))

(provide 'my-org-theme-svg)

;;; my-org-theme-svg.el ends here
