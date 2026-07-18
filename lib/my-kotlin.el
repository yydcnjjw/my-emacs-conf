;;; my-kotlin.el --- Kotlin language server support -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: languages

;; This file is not part of GNU Emacs.

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

;; Register and install the official JetBrains Kotlin language server.

;;; Code:

(require 'lsp-mode)
(require 'url)

(defgroup my/kotlin nil
  "JetBrains Kotlin language server integration."
  :group 'lsp-mode)

(defcustom my/kotlin-lsp-release-api-url
  "https://api.github.com/repos/Kotlin/kotlin-lsp/releases/latest"
  "GitHub API URL used to discover the latest Kotlin LSP release."
  :type 'string
  :group 'my/kotlin)

(defun my/kotlin-lsp--release-label (system architecture)
  "Return the release label for SYSTEM and ARCHITECTURE."
  (pcase (list system architecture)
    (`(gnu/linux x64) "Linux-x64")
    (_ (error "Kotlin LSP automatic installation does not support %s/%s"
              system architecture))))

(defun my/kotlin-lsp--extract-release-url (body release-label)
  "Extract a trusted standalone archive URL from BODY for RELEASE-LABEL."
  (let ((regexp
         (format
          "\\[Download for %s\\](\\(https://download-cdn\\.jetbrains\\.com/language-server/kotlin-server/[^)\n]+\\.tar\\.gz\\))"
          (regexp-quote release-label))))
    (unless (string-match regexp body)
      (error "The latest Kotlin LSP release has no trusted %s archive"
             release-label))
    (match-string 1 body)))

(defun my/kotlin-lsp--latest-release-url ()
  "Return the latest standalone Kotlin LSP URL for this system."
  (let ((buffer (url-retrieve-synchronously
                 my/kotlin-lsp-release-api-url 'silent 'inhibit-cookies)))
    (unless buffer
      (error "Unable to retrieve the latest Kotlin LSP release"))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (error "Invalid response from the Kotlin LSP releases API"))
          (let* ((release (lsp-json-read-buffer))
                 (body (lsp-get release :body))
                 (label (my/kotlin-lsp--release-label
                         system-type
                         (lsp-resolve-value lsp--system-arch))))
            (unless (stringp body)
              (error "The latest Kotlin LSP release has no release body"))
            (my/kotlin-lsp--extract-release-url body label)))
      (kill-buffer buffer))))

(defun my/kotlin-lsp--install-dir ()
  "Return the managed Kotlin LSP installation directory."
  (f-join lsp-server-install-dir "kotlin-lsp"))

(defun my/kotlin-lsp--downloaded-executable ()
  "Return the managed Kotlin LSP launcher path."
  (f-join (my/kotlin-lsp--install-dir) "kotlin-lsp.sh"))

(defun my/kotlin-lsp--command ()
  "Return the command used to start Kotlin LSP."
  (let ((managed (my/kotlin-lsp--downloaded-executable)))
    (list (or (executable-find "kotlin-lsp")
              (and (file-executable-p managed) managed)
              "kotlin-lsp"))))

(defun my/kotlin-lsp--server-present-p ()
  "Return non-nil when a usable Kotlin LSP launcher exists."
  (or (executable-find "kotlin-lsp")
      (file-executable-p (my/kotlin-lsp--downloaded-executable))))

(defun my/kotlin-lsp--download-server
    (_client callback error-callback _update-p)
  "Download Kotlin LSP and invoke CALLBACK or ERROR-CALLBACK."
  (condition-case err
      (lsp-download-install
       (lambda ()
         (let ((launcher (my/kotlin-lsp--downloaded-executable)))
           (if (file-exists-p launcher)
               (progn
                 (set-file-modes launcher #o700)
                 (funcall callback))
             (funcall error-callback
                      (format "Kotlin LSP archive has no %s" launcher)))))
       error-callback
       :url (my/kotlin-lsp--latest-release-url)
       :store-path (f-join (my/kotlin-lsp--install-dir) "kotlin-lsp")
       :decompress :targz)
    (error (funcall error-callback err))))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection #'my/kotlin-lsp--command
                        #'my/kotlin-lsp--server-present-p)
  :major-modes '(kotlin-mode kotlin-ts-mode)
  :priority 1
  :server-id 'kotlin-lsp
  :download-server-fn #'my/kotlin-lsp--download-server))

(provide 'my-kotlin)

;;; my-kotlin.el ends here
