;;; init-rss.el --- rss -*- lexical-binding: t -*-

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

(require 'my-frame)

(use-package elfeed
  :defer t
  :defines
  (elfeed-search-mode-map)
  :bind
  (("C-c e" . elfeed)
   (:map elfeed-search-mode-map
         ("B" . my/elfeed-search-webkit-browse-url)))
  :hook
  ((elfeed-search-mode . olivetti-mode)
   (elfeed-show-mode . olivetti-mode))
  :init
  (setopt shr-inhibit-images t)
  :functions
  (my/elfeed-auto-update)
  :config
  (require 'my-elfeed)

  (advice-add 'elfeed-xml-parse-region :override 'my/elfeed-xml-parse-region)

  ;; FIXME: (void-function (setf (elfeed-meta)))
  ;; (eval-when-compile (require 'elfeed))
  (my/elfeed-auto-update)

  (when (my/is-screen-2k)
    (set-face-attribute 'message-header-subject nil :height 1.2)))


(use-package elfeed-org
  :after elfeed
  :defines (my/elfeed-org-files)
  :commands (elfeed-org)
  :functions (rmh-elfeed-org-process)
  :config
  (setopt rmh-elfeed-org-files my/elfeed-org-files)
  (elfeed-org)
  ;; update elfeed list after loading
  (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id))

;; (use-package elfeed-webkit
;;   :ensure
;;   :after elfeed
;;   :bind (:map elfeed-show-mode-map
;;               ("%" . elfeed-webkit-toggle)))

(provide 'init-rss)

;;; init-rss.el ends here
