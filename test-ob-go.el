;;; test-ob-go.el --- tests for ob-go.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'ert)
(require 'org-id)

(defconst ob-go-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-id-locations-file
  (expand-file-name ".test-org-id-locations" ob-go-test-dir))

(defun ob-go-test-update-id-locations ()
  (let ((files (directory-files
                ob-go-test-dir 'full
                "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$")))
    (org-id-update-id-locations files)))

(defmacro org-test-at-id (id &rest body)
  "Run body after placing the point in the headline identified by ID."
  (declare (indent 1))
  `(let* ((id-location (org-id-find ,id))
	  (id-file (car id-location))
	  (visited-p (get-file-buffer id-file))
	  to-be-removed)
     (unwind-protect
	 (save-window-excursion
	   (save-match-data
	     (org-id-goto ,id)
	     (setq to-be-removed (current-buffer))
	     (condition-case nil
		 (progn
		   (org-show-subtree)
		   (org-show-block-all))
	       (error nil))
	     (save-restriction ,@body)))
       (unless (or visited-p (not to-be-removed))
	 (kill-buffer to-be-removed)))))
(def-edebug-spec org-test-at-id (form body))

(unless (featurep 'ob-go)
  (signal 'missing-test-dependency "Support for Go code blocks"))

(ert-deftest ob-go/assert ()
  (should t))

(ert-deftest ob-go/org-bable-go-package-p ()
  (should (org-babel-go-package-p "package main"))
  (should-not (org-babel-go-package-p "//package main"))

  (should (org-babel-go-package-p "//package main\npackage main"))
  ;; fix it
  ;; (should-not (org-babel-go-package-p "Package main"))
  (should-not (org-babel-go-package-p "packaged")))

(ert-deftest ob-go/simple-program ()
  "Hello world program."
  (if (executable-find org-babel-go-command)
      (org-test-at-id "412a86b1-644a-45b8-9e6d-bdc2b42d7e20"
		      (org-babel-next-src-block 1)
		      (should (= 42 (org-babel-execute-src-block))))))

(ert-deftest ob-go/integer-var ()
  "Test of an integer variable."
  (if (executable-find org-babel-go-command)
      (org-test-at-id "412a86b1-644a-45b8-9e6d-bdc2b42d7e20"
		      (org-babel-next-src-block 2)
		      (should (= 12 (org-babel-execute-src-block))))))

(ert-deftest ob-go/two-variables ()
  "Test of two integer variables."
  (if (executable-find org-babel-go-command)
      (org-test-at-id "412a86b1-644a-45b8-9e6d-bdc2b42d7e20"
		      (org-babel-next-src-block 3)
		      (should (= 666 (org-babel-execute-src-block))))))

(ert-deftest ob-go/two-variables2 ()
  "Test of two integer variables."
  (if (executable-find org-babel-go-command)
      (org-test-at-id "412a86b1-644a-45b8-9e6d-bdc2b42d7e20"
		      (org-babel-next-src-block 4)
		      (should (= 666 (org-babel-execute-src-block))))))

(ert-deftest ob-go/string-variables ()
  "Test the usage of string variables."
  (if (executable-find org-babel-go-command)
      (org-test-at-id "412a86b1-644a-45b8-9e6d-bdc2b42d7e20"
		      (org-babel-next-src-block 5)
		      (should (string-equal "golang" (org-babel-execute-src-block))))))

(ert-deftest ob-go/string-variables ()
  "Test the usage of string variables."
  (if (executable-find org-babel-go-command)
      (org-test-at-id "412a86b1-644a-45b8-9e6d-bdc2b42d7e20"
		      (org-babel-next-src-block 6)
		      (should (string-equal "hello,ob-go" (org-babel-execute-src-block))))))

(ert-deftest ob-go/table ()
  "Test of a table output."
  (if (executable-find org-babel-go-command)
      (org-test-at-id "1e9cf4e3-02df-4f3c-8533-2c0b1ca0a25a"
		      (org-babel-next-src-block 1)
		      (should (equal '((1) (2)) (org-babel-execute-src-block))))))

;; ob-go doesn't handle list variables yet
;; (ert-deftest ob-go/list-var ()
;;   "Test of a list input variable"
;;   (if (executable-find org-babel-go-command)
;;       (org-test-at-id "15000dad-5af1-45e3-ac80-a371335866dc"
;; 		      (org-babel-next-src-block 1)
;; 		      (should (string= "abcdef2" (org-babel-execute-src-block))))))

(ert-deftest ob-go/imports ()
  "Test the imports option"
  (if (executable-find org-babel-go-command)
      (org-test-at-id "e1aaec56-f3c6-4187-a003-5530b3ba956d"
                      (org-babel-next-src-block 1)
                      (should (= 3.141592653589793
                                 (org-babel-execute-src-block))))))

(ert-deftest ob-go/imports ()
  "Test the imports option"
  (if (executable-find org-babel-go-command)
      (org-test-at-id "e1aaec56-f3c6-4187-a003-5530b3ba956d"
                      (org-babel-next-src-block 2)
                      (should (= 3.141592653589793
                                 (org-babel-execute-src-block))))))


(ert-deftest ob-go/packages ()
  (if (executable-find org-babel-go-command)
      (org-test-at-id "c44f7afe-d356-4293-ba83-9ac71c7e6049"
                      (org-babel-next-src-block 1)
                      (should (string-equal "works"
                                            (org-babel-execute-src-block))))))

(ert-deftest ob-go/regression1 ()
  (if (executable-find org-babel-go-command)
      (org-test-at-id "3f63c93d-6f17-478d-9817-e5c24a696689"
                      (org-babel-next-src-block 1)
                      (should (string-equal "'h' and 'i'"
                                            (org-babel-execute-src-block))))))

(defun ob-go-test-runall ()
  (progn
    (ob-go-test-update-id-locations)
    (ert t)))

(provide 'ob-go-test)
