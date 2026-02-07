;;; my-llm-ui.el --- llm ui -*- lexical-binding: t -*-

;; Author: yydcnjjw
;; Maintainer: yydcnjjw

;;; Commentary:

;; UI components for LLM writing tools (side-window, renderer).

;;; Code:

(require 'json)
(require 'dash)

(defconst my/llm-ui-buffer-name-grammar "*my/grammar*")
(defconst my/llm-ui-buffer-name-synonym "*my/synonym*")
(defconst my/llm-ui-buffer-name-improve "*my/improve*")

(defvar my/llm-ui-side-window-width 0.35
  "Width of the side window (fraction of frame width).")

(defun my/llm-ui-display-buffer-in-side-window (buffer)
  "Display BUFFER in a side window."
  (display-buffer buffer
                  `(display-buffer-in-side-window
                    (side . right)
                    (slot . 0)
                    (window-width . ,my/llm-ui-side-window-width)
                    (preserve-size . (t . nil)))))

(defun my/llm-ui-get-buffer-create (name)
  "Get or create buffer with NAME, setting up basic mode."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'special-mode)
        (special-mode))
      (read-only-mode -1)
      (erase-buffer))
    buffer))

(defun my/llm-ui-render-loading (buffer message)
  "Render loading state in BUFFER with MESSAGE."
  (with-current-buffer buffer
    (erase-buffer)
    (insert (propertize "\n\n" 'face '(:height 1.5)))
    (insert (propertize (concat "  ⏳ " message "...")
                        'face '(:height 1.2 :weight bold :foreground "gray")))
    (insert "\n\n  ")
    ;; Placeholder for streaming content
    (display-buffer buffer)))

(provide 'my-llm-ui)
;;; my-llm-ui.el ends here
