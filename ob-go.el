;;; ob-go.el --- org-babel functions for go evaluation

;; Copyright (C) 2012 K. Adam Christensen

;; Author: K. Adam Christensen
;; Keywords: golang, go, literate programming, reproducible research
;; Homepage: http://orgmode.org
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

;; Org-Babel support for evaluating go code.
;;
;; Much of this is modeled after `ob-C'. Just like the `ob-C', you can specify
;; :flags headers when compiling with the "go run" command. Unlike `ob-C', you
;; can also specify :args which can be a list of arguments to pass to the
;; binary. If you quote the value passed into the list, it will use `ob-ref'
;; to find the reference data.
;;
;; If you do not include a main function or a package name, `ob-go' will
;; provide it for you and it's the only way to properly use
;;
;; very limited implementation:
;; - currently only support :results output.
;; - not much in the way of error feedback.
;; - cannot handle table or list input.

;;; Requirements:

;; - You must have go1 installed and the go should be in your `exec-path'. If
;;   not, feel free to modify `org-babel-go-command' to the location of your
;;   go command.
;;
;; - `go-mode' is also recommended for syntax highlighting and
;;   formatting. Not this particularly needs it, it just assumes you
;;   have it.

;;; TODO:

;; - Provide better error feedback.
;;
;; - Figure out a nice way to handle lists and tables.
;;
;; - Do some simple parsing of the go code to insert vars right after the
;;   package declaration.

;;; Code:
(require 'org)
(require 'ob)
(require 'ob-eval)
(require 'ob-ref)


;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("go" . "go"))

(defvar org-babel-default-header-args:go '())

(defvar org-babel-go-command "go"
  "The go command to use to compile and run the go code.")

(defmacro org-babel-go-as-list (val)
  (list 'if (list 'listp val) val (list 'list val)))

(defun org-babel-expand-body:go (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (org-babel-go-get-var params))
         (main-p (not (string= (cdr (assoc :main params)) "no")))
         (imports (or (cdr (assoc :imports params))
                      (org-babel-read (org-entry-get nil "imports" t))))
         (package (or (cdr (assoc :package params))
                      (org-babel-read (org-entry-get nil "package" t))))
         (body (if main-p (org-babel-go-ensure-main-wrap body) body))
         )
    (org-babel-go-custom-vars (org-babel-go-custom-imports (org-babel-go-ensure-package body package)
                                                           imports)
                              vars)))

(defun org-babel-execute:go (body params)
  "Execute a block of Template code with org-babel. This function is
called by `org-babel-execute-src-block'"
  (message "executing Go source code block")
  (let* ((tmp-src-file (org-babel-temp-file "go-src-" ".go"))
         (processed-params (org-babel-process-params params))
         (flags (cdr (assoc :flags processed-params)))
         (args (cdr (assoc :args processed-params)))
         ;; expand the body with `org-babel-expand-body:go'
         (full-body (org-babel-expand-body:go
                     body params processed-params))
         (coding-system-for-read 'utf-8) ;; use utf-8 with subprocesses
         (coding-system-for-write 'utf-8))
    (with-temp-file tmp-src-file (insert full-body))

    (let
	((results
	      (org-babel-eval
	       (format "%s run %s \"%s\" %s"
		       org-babel-go-command
		       (mapconcat 'identity (org-babel-go-as-list flags) " ")
		       (org-babel-process-file-name tmp-src-file)
		       (mapconcat #'(lambda (a)
				      ;; If there's a chance that the symbol is a
				      ;; ref, use that. Otherwise, just return the
				      ;; string form of the value.
				      (format "%S" (if (symbolp a)
						       (let* ((ref (symbol-name a))
							      (out (org-babel-read ref)))
							 (if (equal out ref)
							     (if (string-match "^\".*\"$" ref)
								 (read ref)
							       (org-babel-ref-resolve ref))
							   out))
						     a)))
				  (org-babel-go-as-list args) " ")) "")))
      (if results
	(org-babel-reassemble-table
	 (if (or (member "table" (cdr (assoc :result-params processed-params)))
		 (member "vector" (cdr (assoc :result-params processed-params))))
	     (let ((tmp-file (org-babel-temp-file "go-")))
	       (with-temp-file tmp-file (insert (org-babel-trim results)))
	       (org-babel-import-elisp-from-file tmp-file))
	   (org-babel-read (org-babel-trim results) t))
	 (org-babel-pick-name
	  (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
	 (org-babel-pick-name
	  (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))
	))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:go (session params)
  "This function does nothing as Go is a compiled language with no
support for sessions"
  (error "Go is a compiled languages -- no support for sessions"))

(defun org-babel-go-ensure-main-wrap (body)
  "Check to see if main is already defined. If not, add it."
  (if (string-match-p "^[ \t]*func main *() *{" body)
      body
    (concat "func main() {\n" body "\n}\n")))

(defun org-babel-go-append-package (package)
  "Check to see if package is set. If not, add main unless there is a 'discard value for the package key (allows to tangle many source blocks into one go project)."
  (unless (eq package 'discard)
    (concat "package " (if (and package (not (string-empty-p package))) package "main"))))

(defun org-babel-go-ensure-package (body package)
  "Ensure package exists."
  (if (org-babel-go-package-p body)
      body
    (concat (org-babel-go-append-package package) "\n" body)))

(defun org-babel-go-package-p (body)
  "Check to see whether package is set or not."
  (string-match-p "^[ \t]*package " body))

(defun org-babel-go-package-position (body)
  (string-match "^[ \t]*package " body))

(defun org-babel-go-custom-imports (body imports)
  "Append custom import packages."
  (let* ((start (string-match "\n"
                              (substring body
                                         (org-babel-go-package-position body)))))
    (concat (substring body 0 start)
            "\n"
            (org-babel-go-insert-imports imports)
            (substring body start))))

(defun org-babel-go-insert-imports (imports)
  (let ((packages (org-babel-go-as-list imports)))
    (if (= (length packages) 0)
        ""
      (concat "import ("
              "\n\t"
              (mapconcat #'(lambda (pkg) (format "%S" pkg))
                         packages
                         "\t\n")
              "\n)"
              "\n"))))

(defun org-babel-go-custom-vars (body vars)
  "Append custom variables at bottom."
  (if (=  (length vars) 0)
      body
    (concat body "\n" (mapconcat 'org-babel-go-var-to-go vars "\n"))))

(defun org-babel-go-get-var (params)
  "org-babel-get-header was removed in org version 8.3.3"
  (let* ((fversion (org-version))
         (version (string-to-number fversion)))
    (if (< version 8.3)
        (mapcar #'cdr (org-babel-get-header params :var))
      (org-babel--get-vars params))))

(defun org-babel-go-gofmt (body)
  "Run gofmt over the body. Why not?"
  (with-temp-buffer
    (let ((outbuf (current-buffer))
          (coding-system-for-read 'utf-8) ;; use utf-8 with subprocesses
          (coding-system-for-write 'utf-8))
      (with-temp-buffer
        (insert body)
        (shell-command-on-region (point-min) (point-max) "gofmt"
                                 outbuf nil nil)))
    (buffer-string)))

(defun org-babel-go-var-to-go (pair)
  "Convert an elisp var into a string of go source code
specifying a var of the same value."
  (let ((var (car pair))
        (val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val)))
    ;; TODO(pope): Handle tables and lists.
    (format "var %S = %S" var val)))

(provide 'ob-go)
;;; ob-go.el ends here
