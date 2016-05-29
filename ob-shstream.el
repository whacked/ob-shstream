;;; ob-shstream.el --- org-babel functions for simple async shell eval

;; Copyright (C) your name here

;; Author: your name here
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

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

;; the default ob-sh evaluation for shell scripts in org-mode is
;; blocking, so commands like e.g.
;; 
;; #+BEGIN_SRC sh
;; ping -c 5 127.0.0.1
;; #+END_SRC
;;
;; will block when run, and results only become visible when the
;; process terminates. :session may be able to hand this, but it
;; doesn't currently work for me OOTB. ob-shstream is a simple
;; workaround.
;; 
;; It returns a process buffer name immediately on eval, saves the
;; eval contents to a shell script in `org-babel-temporary-directory`
;; and executes it with `bash`. org-babel thus outputs the unfinished
;; process buffer name as the result, and the process buffer is shown
;; with output asynchronously appended. When the process terminates,
;; the output replaces the unfinished process name result as the final
;; org-babel #+RESULTS section.
;;
;; NOTE: header arguments (e.g. session) are not supported
;; 
;; async update procedure modified from http://www.drieu.org/code/sources/tail.el
;; also see:
;; http://orgmode.org/w/worg.git/blob/HEAD:/org-contrib/babel/ob-template.el
;; http://orgmode.org/w/?p=org-mode.git;a=blob_plain;f=lisp/ob-eval.el

;;; Usage:

;; load this file and update org-babel settings:
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(...
;;    (shstream . t)
;;    ...))
;; then use like a normal sh block, but replace "sh" with "shstream"

;;; Requirements:

;; defaults to evaluation using `bash`

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
;; possibly require modes required for your language

(defvar org-babel-shstream-command "bash"
  "Name of command for executing shstream code.")

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:shstream '())

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:shstream' function below.
(defun org-babel-expand-body:shstream (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
    ; do nothing now
    )
  )

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:shstream (body params)
  "Execute a block of sh code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing shstream source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the session variable is non-nil
         ;; (session (org-babel-shstream-initiate-session (first processed-params)))
         ;; variables assigned for use in the block
         (vars (second processed-params))
         (result-params (third processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (fourth processed-params))
         ;; expand the body with `org-babel-expand-body:shstream'
         (full-body (org-babel-expand-body:shstream
                     body params processed-params)))
    ;; actually execute the source-code block either in a session or
    ;; possibly by dropping it to a temporary file and evaluating the
    ;; file.
    ;; 
    ;; for session based evaluation the functions defined in
    ;; `org-babel-comint' will probably be helpful.
    ;;
    ;; for external evaluation the functions defined in
    ;; `org-babel-eval' will probably be helpful.
    ;;
    ;; when forming a shell command, or a fragment of code in some
    ;; other language, please preprocess any file names involved with
    ;; the function `org-babel-process-file-name'. (See the way that
    ;; function is used in the language files)

    ;; eval content
    (lexical-let* ((hash-string (secure-hash 'md5 body))
                   (process-buffer-name (concat "*emacs-process-" hash-string "-<waiting>*"))
                   (tempfile-path (concat org-babel-temporary-directory "/process-" hash-string ".sh")))

      ;; (insert (format "\n#+RESULTS:\n: %s\n" process-buffer-name))
      (write-region (concat "\n" body "\n") nil tempfile-path 'append)

      (let ((process 
             (apply 'start-process-shell-command
                    (concat "shstream-" hash-string)
                    process-buffer-name
                    org-babel-shstream-command
                    (list tempfile-path))))     
        
        (set-process-filter process 'shstream-filter)
        
        (set-process-sentinel
         process
         (lambda (p e)
           (let ((buffer (process-buffer p)))
             (when (not (null buffer))
               ;; (message process-buffer-name)
               (save-excursion
                 (beginning-of-buffer)
                 ;; initially used this:
                 ;; (replace-string
                 ;;  (concat ": " process-buffer-name) ...)
                 ;; which should work, but to be extra sure
                 ;; we'll use a match-all from beginning-of-line
                 (when (re-search-forward
                        (concat "^.*"
                                ;; escape the earmuffs
                                (replace-regexp-in-string "\\*" "\\\\*" process-buffer-name)
                                "$") nil t)
                   (replace-match
                    (concat "#+begin_example\n"
                            (with-current-buffer (get-buffer process-buffer-name)
                              (buffer-string))
                            "\n#+end_example\n")))
                 )
               (delete-window (get-buffer-window process-buffer-name))
               (kill-buffer process-buffer-name)
               (delete-file tempfile-path)))))
        ;; process ;; return process directly (for debug)
        process-buffer-name
        )
      )
    ))

(provide 'ob-shstream)
;;; ob-shstream.el ends here
