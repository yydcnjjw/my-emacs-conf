;;; my-company.el --- company -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Package-Requires: (company company-quickhelp)


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

(use-package company-tabnine
  :after company
  :config
  (defun my/company/sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-1
            candidates-2)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-2))
            (push candidate candidates-1)
            (puthash candidate t candidates-table)))
        (setq candidates-1 (nreverse candidates-1))
        (setq candidates-2 (nreverse candidates-2))
        (nconc (seq-take candidates-1 2)
               (seq-take candidates-2 2)
               (seq-drop candidates-1 2)
               (seq-drop candidates-2 2)))))

  (add-to-list 'company-transformers 'my/company/sort-by-tabnine t)

  (defun my/tabnine-installed-p ()
    (condition-case nil
        (company-tabnine--executable-path)
      (error nil))
    )
  )

(use-package company
  :defer t
  :hook
  ((prog-mode text-mode conf-mode) . company-mode)
  :config
  (progn
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t
          company-idle-delay 0.2
          company-tooltip-limit 10
          company-tooltip-flip-when-above t
          company-minimum-prefix-length 1
          company-backends `(company-cmake
                             company-files
                             ;; ,(if (my/tabnine-installed-p)
                             ;;      '(company-capf :with company-tabnine :separate)
                             ;;    'company-capf)
                             company-capf
                             company-keywords
                             company-dabbrev)))
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :diminish company-mode)

(provide 'my-company)

;;; my-company.el ends here
