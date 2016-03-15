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
(require 'org-id)
(require 'cl)
(require 'ox)

(defconst ob-go-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-id-locations-file
  (expand-file-name ".test-org-id-locations" ob-go-test-dir))

(defun ob-go-test-update-id-locations ()
  (org-id-update-id-locations
   (directory-files
    ob-go-test-dir 'full
    "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$")))

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

(defun ob-go-test-runall ()
  (interactive)
  (progn
    (ob-go-test-update-id-locations)
    (ert t)))

(unless (featurep 'ob-go)
  (signal 'missing-test-dependency "Support for Go code blocks"))

(ert-deftest ob-go/assert ()
  (should t))

(ert-deftest ob-go/simple-program ()
  "Hello world program."
  (if (executable-find org-babel-go-compiler)
      (org-test-at-id "412a86b1-644a-45b8-9e6d-bdc2b42d7e20"
		      (org-babel-next-src-block 1)
		      (should (= 42 (org-babel-execute-src-block))))))

(provide 'ob-go-test)
