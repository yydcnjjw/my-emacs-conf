;;; my-kotlin-test.el --- Tests for my-kotlin -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'my-kotlin)

(defconst my/kotlin-test-root-dir
  (file-name-directory
   (directory-file-name (file-name-directory load-file-name))))

(defconst my/kotlin-test-release-body
  "Standalone archive\n\
[Download for Linux-x64](https://download-cdn.jetbrains.com/language-server/kotlin-server/262.8190.0/kotlin-server-262.8190.0.tar.gz)\n\
[Download for Linux-arm64](https://download-cdn.jetbrains.com/language-server/kotlin-server/262.8190.0/kotlin-server-262.8190.0-aarch64.tar.gz)")

(ert-deftest my/kotlin-lsp-release-label-linux-x64 ()
  (should (equal (my/kotlin-lsp--release-label 'gnu/linux 'x64)
                 "Linux-x64")))

(ert-deftest my/kotlin-lsp-release-label-rejects-unsupported-target ()
  (should-error (my/kotlin-lsp--release-label 'darwin 'arm64)
                :type 'error))

(ert-deftest my/kotlin-lsp-extracts-standalone-linux-url ()
  (should
   (equal
    (my/kotlin-lsp--extract-release-url
     my/kotlin-test-release-body "Linux-x64")
    "https://download-cdn.jetbrains.com/language-server/kotlin-server/262.8190.0/kotlin-server-262.8190.0.tar.gz")))

(ert-deftest my/kotlin-lsp-rejects-missing-release-link ()
  (should-error
   (my/kotlin-lsp--extract-release-url "No archive" "Linux-x64")
   :type 'error))

(ert-deftest my/kotlin-lsp-rejects-untrusted-release-link ()
  (should-error
   (my/kotlin-lsp--extract-release-url
    "[Download for Linux-x64](https://example.com/kotlin-server.tar.gz)"
    "Linux-x64")
   :type 'error))

(ert-deftest my/kotlin-lsp-command-prefers-system-executable ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_command) "/usr/local/bin/kotlin-lsp"))
            ((symbol-function 'file-executable-p) (lambda (_path) t)))
    (should (equal (my/kotlin-lsp--command)
                   '("/usr/local/bin/kotlin-lsp")))))

(ert-deftest my/kotlin-lsp-command-uses-managed-executable ()
  (cl-letf (((symbol-function 'executable-find) (lambda (_command) nil))
            ((symbol-function 'file-executable-p) (lambda (_path) t))
            ((symbol-function 'my/kotlin-lsp--downloaded-executable)
             (lambda () "/tmp/kotlin-lsp/kotlin-lsp.sh")))
    (should (equal (my/kotlin-lsp--command)
                   '("/tmp/kotlin-lsp/kotlin-lsp.sh")))))

(ert-deftest my/kotlin-lsp-command-falls-back-to-command-name ()
  (cl-letf (((symbol-function 'executable-find) (lambda (_command) nil))
            ((symbol-function 'file-executable-p) (lambda (_path) nil)))
    (should (equal (my/kotlin-lsp--command) '("kotlin-lsp")))))

(ert-deftest my/kotlin-lsp-client-is-registered ()
  (let ((client (gethash 'kotlin-lsp lsp-clients)))
    (should client)
    (should (= (lsp--client-priority client) 1))
    (should (equal (lsp--client-major-modes client)
                   '(kotlin-mode kotlin-ts-mode)))
    (should (eq (lsp--client-download-server-fn client)
                #'my/kotlin-lsp--download-server))))

(ert-deftest my/kotlin-lsp-download-marks-launcher-executable ()
  (let ((launcher (make-temp-file "kotlin-lsp-test-"))
        callback-called)
    (unwind-protect
        (cl-letf (((symbol-function 'my/kotlin-lsp--latest-release-url)
                   (lambda () "https://download-cdn.jetbrains.com/language-server/kotlin-server/262.8190.0/kotlin-server-262.8190.0.tar.gz"))
                  ((symbol-function 'my/kotlin-lsp--downloaded-executable)
                   (lambda () launcher))
                  ((symbol-function 'lsp-download-install)
                   (lambda (callback _error-callback &rest _arguments)
                     (funcall callback))))
          (my/kotlin-lsp--download-server
           nil (lambda () (setq callback-called t)) #'ert-fail nil)
          (should callback-called)
          (should (= (file-modes launcher) #o700)))
      (delete-file launcher))))

(ert-deftest my/kotlin-lsp-is-loaded-by-language-configuration ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents
            (expand-file-name "config/init-prog-language.el"
                              my/kotlin-test-root-dir))
           (buffer-string))))
    (should (string-match-p
             "(use-package kotlin-ts-mode\\(?:.\\|\n\\)*:config\\(?:.\\|\n\\)*(require 'my-kotlin)"
             source))))

(provide 'my-kotlin-test)

;;; my-kotlin-test.el ends here
