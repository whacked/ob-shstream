;;; ob-shstream.el --- org-babel functions for simple async shell eval -*- lexical-binding: t -*-

;; Copyright (C) 2021 Excalamus (excalamus.com), whacked
;; (https://github.com/whacked)

;; Author: Excalamus, whacked (https://github.com/whacked)
;; Keywords: literate programming, reproducible research
;; Version: 0.02

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The default ob-sh evaluation for shell scripts in org-mode is
;; blocking, so commands like the following freeze Emacs:
;;
;; #+BEGIN_SRC sh
;; ping -c 5 127.0.0.1
;; #+END_SRC
;;
;; Results only become visible when the process terminates.
;;
;; ob-shstream.el works by running the source given in a shell block
;; in a separate process. It does this by creating a temporary file
;; and executing it. Output is appended to a buffer and a hash
;; representing the source code acts as the result until the process
;; finishes. When completed, the hash is replaced with the output
;; buffer contents.
;;
;; NOTE: header arguments (e.g. :session) are not supported and some
;; may not work as expected (e.g. :table). This was only tested with
;; :result output. This is because the org-babel functionality is
;; largely undocumented and it's API inconsistent.

;;; Usage:

;; Load this file and update the org-babel settings:
;;
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(...
;;    (shstream . t)
;;    ...))
;;
;; then use like a normal sh block, but replace "sh" with "shstream".
;;
;; Example:
;;
;;     #+begin_src shstream :results output
;;     echo "hello, world!"
;;     sleep 2
;;     echo "goodbye, cruel world..."
;;     #+end_src
;;
;; Check the source code for user variables.

;;; Requirements:

;;; Code:

(defvar ob-shstream-shell-command "bash"
  "Command used to execute code within a shstream block.

It is assumed that the command is on the shell path (i.e. that it
can be called from the shell without providing the path to the
executable).")

(defun ob-shstream-default-insertion-filter (proc string)
  "Insert process output in process buffer.

Taken from (elisp) Filter Functions."
  (when (buffer-live-p (process-buffer proc))
    (display-buffer (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))))))

(defvar ob-shstream-filter-function 'ob-shstream-default-insertion-filter
  "Filter function to apply to process output before it is
inserted into the process output buffer.")

(defun org-babel-execute:shstream-pass-through (body params)
  "Execute a block of shstream-pass-through code with org-babel.

This function is called by `org-babel-execute-src-block' This
particular definition is a dummy source language used strictly to
format shstream output."
  body)

(defun org-babel-execute:shstream (body params)
  "Execute a block of shstream code with org-babel.

This function is called by `org-babel-execute-src-block'"
  (let* ((org-buffer (current-buffer))
         (hash (secure-hash 'sha256 body))
         (hash-name (concat "ob-shstream-" hash))
         (short-hash-name (concat "ob-shstream-" (substring hash 0 7)))
         (process-output-buffer-name (concat "*" short-hash-name "*"))
         (temp-shell-file (org-babel-temp-file hash-name ".sh"))
         ;; (info (org-babel-get-src-block-info))
         (results-params (cdr (assq :results params)))
         (session-name (cdr (assq :session params))))

    ;; shell source must be in a temp file to be run asynchronously
    (with-temp-file temp-shell-file
      (insert body))

    (with-current-buffer (get-buffer-create process-output-buffer-name)
      (display-buffer process-output-buffer-name)
      (read-only-mode nil))

    (let* ((process (make-process
                     :name hash-name
                     :buffer process-output-buffer-name
                     :command (list ob-shstream-shell-command temp-shell-file)
                     ;; no need to communicate between processes; kill
                     ;; with list-processes if needed
                     :connection 'pipe
                     :filter ob-shstream-filter-function
                     :sentinel `(lambda (process event)
                                  (if (string= event "finished\n")
                                      (progn

                                        ;; run process output through shstream-pass-through in order to format
                                        ;; (e.g. put colons in front of each line)
                                        (let ((formatted-result
                                               (with-temp-buffer
                                                 ;; DEBUG:
                                                 ;; (with-current-buffer (get-buffer-create "*shstream-pass-through*")
                                                 ;;   (erase-buffer)
                                                 (insert (format "#+begin_src shstream-pass-through :results %s :session %s\n%s#+end_src"
                                                                 ,results-params
                                                                 ,session-name
                                                                 (with-current-buffer ,process-output-buffer-name
                                                                   (buffer-string))))
                                                 (org-babel-execute-src-block)
                                                 ;; get the org formatted result
                                                 (let ((result-start (+ (org-babel-where-is-src-block-result) 10)))
                                                   (buffer-substring-no-properties result-start (point-max))))))

                                          ;; replace hash with formatted results
                                          (save-excursion
                                            (save-restriction
                                              (with-current-buffer ,org-buffer
                                                (widen)
                                                (beginning-of-buffer)
                                                (re-search-forward ,hash nil t)
                                                ;; assumes no one has put any text between the results
                                                ;; and the source block which contains "shstream"
                                                (goto-char (re-search-backward "shstream" nil t))

                                                (let* ((results-start (+ (org-babel-where-is-src-block-result) 10))
                                                       (results-end (when results-start
                                                                      (save-excursion
                                                                        (goto-char results-start)
                                                                        (goto-char (org-babel-result-end))
                                                                        (point)))))
                                                  (delete-region results-start results-end)
                                                  (goto-char results-start)
                                                  (insert formatted-result)
                                                  ;; this is probably annoying, but that's Emacs window/buffer
                                                  ;; management for ya
                                                  (switch-to-prev-buffer (get-buffer-window ,process-output-buffer-name) t)
                                                  (kill-buffer ,process-output-buffer-name)
                                                  (delete-file ,temp-shell-file)))))))))))))

    ;; use hash as place holder until process completes
    hash))

(provide 'ob-shstream)
;;; ob-shstream.el ends here
