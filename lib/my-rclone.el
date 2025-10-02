;;; my-rclone.el --- rclone -*- lexical-binding: t -*-

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

(defcustom my/rclone-binary "rclone"
  "Rclone binary."
  :type 'string
  :group 'my)

(defcustom my/rclone-vfs-cache-mode "full"
  "Rclone args vfs-cache-mode."
  :type 'string
  :group 'my)

(defcustom my/rclone-vfs-links t
  "Rclone args vfs-links."
  :type 'string
  :group 'my)

(defcustom my/rclone-allow-non-empty t
  "Rclone args allow-none-empty."
  :type 'string
  :group 'my)

(defvar my/rclone--server-process-list (make-hash-table :test 'equal)
  "The mu-server process.")

(defun my/rclone--kill-stale (name)
  "Kill stale rclone server process with NAME."
  (seq-each
   (lambda(proc)
     (when (and (process-live-p proc)
                (string-prefix-p name (process-name proc)))
       (message "killing stale %s rclone server" name)
       (ignore-errors
         (signal-process proc 'SIGINT) ;; nicely
         (sit-for 1.0)
         (signal-process proc 'SIGKILL)))) ;; forcefully
   (process-list)))

(defun my/rclone--server-args (remote local)
  "Rclone server args with REMOTE and LOCAL."
  (seq-filter #'identity ;; filter out nil
              `("mount"
                ,(when my/rclone-allow-non-empty "--allow-non-empty")
                ,(when my/rclone-vfs-links "--vfs-links")
                ,(when my/rclone-vfs-cache-mode (format "--vfs-cache-mode=%s" my/rclone-vfs-cache-mode))
                ,remote
                ,local)))

(defun my/rclone--server-sentinel (proc _msg)
  "Function called when the server process PROC terminates with MSG."
  (let ((status (process-status proc))
        (code (process-exit-status proc))
        (name (process-name proc)))
    (remhash name my/rclone--server-process-list)
    (cond
     ((eq status 'exit)
      (cond
       ((eq code 0)
        (message nil)) ;; don't do anything
       (t (error "Rclone server process ended with exit code %d" code))))
     (t
      (error "Something bad happened to the rclone server process")))))

(defun my/rclone-server-start (name remote local)
  "Start the rclone server process with NAME REMOTE LOCAL."
  (my/rclone--kill-stale name)
  (let* ((process-connection-type nil) ;; use a pipe
         (args (my/rclone--server-args remote local))
         (process (apply 'start-process
                         name name
                         my/rclone-binary args)))
    (unless process
      (error "Failed to start the rclone with %s" name))

    (puthash name process my/rclone--server-process-list)
    (set-process-query-on-exit-flag process nil)
    (set-process-coding-system process 'binary 'utf-8-unix)
    (set-process-sentinel process 'my/rclone--server-sentinel)))

(provide 'my-rclone)

;;; my-rclone.el ends here
