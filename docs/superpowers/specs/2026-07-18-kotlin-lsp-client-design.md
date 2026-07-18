# Kotlin LSP Client Design

## Goal

Add first-class `lsp-mode` support for JetBrains' official
`Kotlin/kotlin-lsp` server. Kotlin buffers should prefer the official server,
use an existing `kotlin-lsp` executable when available, and support installing
the latest standalone Linux archive through `lsp-install-server`.

## Scope

- Add the client implementation in `lib/my-kotlin.el`.
- Load that implementation from the existing `kotlin-ts-mode` configuration in
  `config/init-prog-language.el`.
- Support the current Linux x86_64 environment for automatic downloads.
- Keep platform and architecture selection isolated so additional release
  targets can be added without changing the client registration.
- Preserve unrelated edits in `config/init-prog-language.el`.

Automatic download support for other operating systems and architectures is
out of scope. An unsupported target must fail with an actionable error instead
of downloading an incompatible archive.

## Client Registration

`my-kotlin.el` will require `lsp-mode` and register a client with server ID
`kotlin-lsp` for `kotlin-mode` and `kotlin-ts-mode`.

The client will have a higher priority than lsp-mode's built-in
`kotlin-ls` client, which targets the older `fwcd/kotlin-language-server`.
Its stdio command resolver will use:

1. `kotlin-lsp` found on `exec-path`.
2. The downloaded `kotlin-lsp.sh` under `lsp-server-install-dir`.
3. The literal command `kotlin-lsp`, allowing lsp-mode to report its normal
   missing-server error and offer installation.

No extra command-line arguments are required because the official launcher
starts the language server over stdio by default.

## Latest-Release Resolution and Installation

The download callback registered on the client will:

1. Retrieve the latest release metadata from GitHub's releases API.
2. Read the Markdown release body, because JetBrains publishes standalone
   archives on its CDN rather than as GitHub release assets.
3. Select the standalone Linux x86_64 `.tar.gz` URL associated with the
   release's `Download for Linux-x64` entry.
4. Pass the selected URL to `lsp-download-install` and extract it below a
   Kotlin-specific directory in `lsp-server-install-dir`.
5. Mark the extracted `kotlin-lsp.sh` executable before invoking lsp-mode's
   success callback.

Release parsing will validate both the URL host and expected archive path. A
missing or malformed entry will call the provided error callback with a useful
message. Network and decompression failures remain delegated to lsp-mode's
download machinery.

Re-running `lsp-install-server` for an installed client will use lsp-mode's
normal update behavior and resolve the latest release again.

## Configuration Integration

The existing `use-package kotlin-ts-mode` form will load `my-kotlin` in its
configuration phase. Its file associations, tree-sitter grammar registration,
and `my/lsp-register-major-mode` hook remain in place.

This keeps language activation in `config/init-prog-language.el` and confines
server-specific details to `lib/my-kotlin.el`.

## Tests and Verification

ERT tests will cover:

- extracting the expected standalone URL from representative release JSON;
- rejecting missing, malformed, or wrong-host URLs;
- selecting Linux x86_64 and rejecting unsupported targets;
- preferring a system executable over the downloaded launcher;
- resolving the downloaded launcher when no system executable exists;
- registering the client for Kotlin modes with the intended priority and
  download callback.

Verification will run the ERT suite in batch mode, load the affected Emacs Lisp
files in batch mode, and byte-compile the new client while treating warnings as
errors where the local configuration permits it. A real server download is not
part of automated verification because it mutates the user's LSP cache and
depends on the network; the parsing and installation callback boundary will be
tested without performing the download.
