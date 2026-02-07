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

(defun my/writing-assist-execute-action ()
  "Execute action at point."
  (interactive)
  (let ((action (get-text-property (point) 'my-writing-assist-action))
        (data (get-text-property (point) 'my-writing-assist-data)))
    (if (and action data)
        (funcall action data)
      (user-error "No action at point"))))

(defun my/writing-assist-next-item ()
  "Move to next actionable item."
  (interactive)
  (let ((next (text-property-search-forward 'my-writing-assist-action nil nil)))
    (when next
      (goto-char (prop-match-beginning next)))))

(defun my/writing-assist-prev-item ()
  "Move to previous actionable item."
  (interactive)
  (let ((prev (text-property-search-backward 'my-writing-assist-action nil nil)))
    (when prev
      (goto-char (prop-match-beginning prev)))))

(defun my/writing-assist-execute-number (n)
  "Execute action for item number N."
  (interactive "nItem number: ")
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (found nil))
      (while (and (not found)
                  (let ((match (text-property-search-forward 'my-writing-assist-action nil nil)))
                    (when match
                      (setq count (1+ count))
                      (goto-char (prop-match-beginning match)) ;; Ensure we are at start for execution
                      (if (= count n)
                          (setq found t)
                        (goto-char (prop-match-end match)) ;; Continue searching
                        t)))))
      (if found
          (my/writing-assist-execute-action)
        (user-error "Item %d not found" n)))))

(defvar my-writing-assist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my/writing-assist-execute-action)
    (define-key map (kbd "n") #'my/writing-assist-next-item)
    (define-key map (kbd "p") #'my/writing-assist-prev-item)
    (define-key map (kbd "q") #'quit-window)
    (dotimes (i 9)
      (let ((n (1+ i)))
        (define-key map (kbd (number-to-string n))
                    (lambda () (interactive) (my/writing-assist-execute-number n)))))
    map)
  "Keymap for `my-writing-assist-mode'.")

(define-derived-mode my-writing-assist-mode special-mode "WritingAssist"
  "Major mode for writing assistant side buffers."
  (setq-local cursor-type 'box))

(defun my/llm-ui-get-buffer-create (name)
  "Get or create buffer with NAME, setting up basic mode."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'my-writing-assist-mode)
        (my-writing-assist-mode))
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
