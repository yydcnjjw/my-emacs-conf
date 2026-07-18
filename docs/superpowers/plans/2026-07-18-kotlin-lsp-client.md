# Kotlin LSP Client Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Register JetBrains' official Kotlin language server with `lsp-mode`, prefer it for Kotlin buffers, and install its latest Linux x86_64 standalone release through `lsp-install-server`.

**Architecture:** A focused `lib/my-kotlin.el` module owns release discovery, archive installation, executable resolution, and client registration. The existing Kotlin `use-package` form loads this module, while ERT tests isolate release parsing and command selection from the network and filesystem.

**Tech Stack:** Emacs Lisp, `lsp-mode`, `url`, `f.el`, ERT, GitHub Releases API, JetBrains standalone Kotlin LSP archive.

## Global Constraints

- Automatic downloads support Linux x86_64 only.
- Unsupported platforms and architectures fail before downloading.
- A `kotlin-lsp` executable on `exec-path` takes precedence over the managed copy.
- The official client has higher priority than lsp-mode's built-in `kotlin-ls` client.
- Preserve unrelated edits in `config/init-prog-language.el` and the untracked `%backup%~` file.
- Tests must not download a real server or mutate the user's LSP cache.

## File Structure

- Create `lib/my-kotlin.el`: release discovery, installation, executable resolution, and client registration.
- Create `test/my-kotlin-test.el`: isolated ERT coverage for the new module.
- Modify `config/init-prog-language.el`: load the module from the existing Kotlin form.

---

### Task 1: Release discovery and validation

**Files:**
- Create: `test/my-kotlin-test.el`
- Create: `lib/my-kotlin.el`

**Interfaces:**
- Consumes: `url-retrieve-synchronously`, `lsp-json-read-buffer`, `lsp-get`, `lsp--system-arch`.
- Produces: `my/kotlin-lsp--release-label`, `my/kotlin-lsp--extract-release-url`, `my/kotlin-lsp--latest-release-url`.

- [ ] **Step 1: Write the failing release tests**

Create `test/my-kotlin-test.el`:

```elisp
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

(provide 'my-kotlin-test)
;;; my-kotlin-test.el ends here
```

- [ ] **Step 2: Run the test and verify the missing module failure**

Run:

```bash
emacs --batch -Q \
  --eval '(let ((default-directory (expand-file-name "straight/build" user-emacs-directory))) (normal-top-level-add-subdirs-to-load-path))' \
  -L lib -L test -l test/my-kotlin-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL because `my-kotlin` does not exist.

- [ ] **Step 3: Implement the minimal release resolver**

Create `lib/my-kotlin.el` with the repository's standard header followed by:

```elisp
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

(provide 'my-kotlin)
;;; my-kotlin.el ends here
```

- [ ] **Step 4: Run the focused test and verify all five tests pass**

Run the command from Step 2. Expected: 5 tests, 5 passed, 0 unexpected.

- [ ] **Step 5: Commit the release resolver**

```bash
git add lib/my-kotlin.el test/my-kotlin-test.el
git commit -m "feat(kotlin): resolve official LSP releases"
```

### Task 2: Managed installation and client registration

**Files:**
- Modify: `test/my-kotlin-test.el`
- Modify: `lib/my-kotlin.el`

**Interfaces:**
- Consumes: release resolver from Task 1 plus `lsp-download-install`, `lsp-register-client`, `make-lsp-client`, and `lsp-stdio-connection`.
- Produces: `my/kotlin-lsp--downloaded-executable`, `my/kotlin-lsp--command`, `my/kotlin-lsp--server-present-p`, `my/kotlin-lsp--download-server`, and server ID `kotlin-lsp`.

- [ ] **Step 1: Add failing command and registration tests**

Insert before the existing `provide` and footer in `test/my-kotlin-test.el`:

```elisp
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
```

- [ ] **Step 2: Run the suite and verify undefined-function failures**

Run the Task 1 test command. Expected: the new tests fail because the command and client functions are missing.

- [ ] **Step 3: Implement managed installation and registration**

Insert before the existing `provide` and footer in `lib/my-kotlin.el`:

```elisp
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
```

- [ ] **Step 4: Add an isolated download-callback test**

Append to `test/my-kotlin-test.el` before its footer:

```elisp
(ert-deftest my/kotlin-lsp-download-marks-launcher-executable ()
  (let (mode callback-called)
    (cl-letf (((symbol-function 'my/kotlin-lsp--latest-release-url)
               (lambda () "https://download-cdn.jetbrains.com/language-server/kotlin-server/262.8190.0/kotlin-server-262.8190.0.tar.gz"))
              ((symbol-function 'my/kotlin-lsp--downloaded-executable)
               (lambda () "/tmp/kotlin-lsp/kotlin-lsp.sh"))
              ((symbol-function 'file-exists-p) (lambda (_path) t))
              ((symbol-function 'set-file-modes)
               (lambda (_path value) (setq mode value)))
              ((symbol-function 'lsp-download-install)
               (lambda (callback _error-callback &rest _arguments)
                 (funcall callback))))
      (my/kotlin-lsp--download-server
       nil (lambda () (setq callback-called t)) #'ert-fail nil)
      (should callback-called)
      (should (= mode #o700)))))

```

- [ ] **Step 5: Run all ten tests and check formatting**

Run the Task 1 test command. Expected: 10 tests, 10 passed, 0 unexpected.

Run `git diff --check -- lib/my-kotlin.el test/my-kotlin-test.el`. Expected: no output.

- [ ] **Step 6: Commit the client and installer**

```bash
git add lib/my-kotlin.el test/my-kotlin-test.el
git commit -m "feat(kotlin): register official LSP client"
```

### Task 3: Kotlin mode integration and verification

**Files:**
- Modify: `config/init-prog-language.el:112`
- Test: `test/my-kotlin-test.el`

**Interfaces:**
- Consumes: feature `my-kotlin` from Task 2 and the existing `kotlin-ts-mode` form.
- Produces: client registration before the existing Kotlin LSP hook runs.

- [ ] **Step 1: Add the failing integration test**

Append before the test file footer:

```elisp
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
```

Run the Task 1 test command. Expected: this test fails because the Kotlin form does not load `my-kotlin`.

- [ ] **Step 2: Load the client in the existing Kotlin form**

Change only the end of the existing `use-package kotlin-ts-mode` form:

```elisp
  (my/treesit-register
   '(:lang kotlin
           :source ("https://github.com/fwcd/tree-sitter-kotlin" "main" "src")
           :mode (kotlin-ts-mode)))
  :config
  (require 'my-kotlin))
```

- [ ] **Step 3: Run the complete suite**

Run the Task 1 test command. Expected: 11 tests, 11 passed, 0 unexpected.

- [ ] **Step 4: Byte-compile and load the client**

Run:

```bash
emacs --batch -Q \
  --eval '(let ((default-directory (expand-file-name "straight/build" user-emacs-directory))) (normal-top-level-add-subdirs-to-load-path))' \
  -L lib -L config \
  --eval '(setq byte-compile-error-on-warn t)' \
  --eval '(byte-compile-file "lib/my-kotlin.el")'
```

Expected: exit 0 with no warnings. Delete only the generated `lib/my-kotlin.elc` afterward.

Run:

```bash
emacs --batch -Q \
  --eval '(let ((default-directory (expand-file-name "straight/build" user-emacs-directory))) (normal-top-level-add-subdirs-to-load-path))' \
  -L lib -L config -l lib/my-kotlin.el \
  --eval '(unless (gethash (quote kotlin-lsp) lsp-clients) (kill-emacs 1))'
```

Expected: exit 0.

- [ ] **Step 5: Review the exact diff**

Run:

```bash
git diff --check
git diff -- lib/my-kotlin.el test/my-kotlin-test.el config/init-prog-language.el
git status --short
```

Expected: the Markdown edit and `%backup%~` remain untouched. The Kotlin form contains only the intended loader addition beyond the user's earlier Kotlin work.

- [ ] **Step 6: Commit only separable integration work**

Use an interactive staged diff to include only the two new `:config` lines, then run:

```bash
git diff --cached --check
git commit -m "feat(kotlin): enable official LSP client"
```

If those lines cannot be separated safely from the pre-existing Kotlin hunk, leave `config/init-prog-language.el` uncommitted and report the mixed user changes instead of committing them together.
