.PHONY: test

test:
	@rm -f .test-org-id-locations
	emacs -Q --batch -q \
		-L . \
		-l ob-go.el \
		-l test-ob-go.el \
		--eval "(progn \
	              (setq org-confirm-babel-evaluate nil) \
	              (org-babel-do-load-languages \
	                'org-babel-load-languages '((emacs-lisp . t) \
	                                            (sh . t) \
	                                            (org . t) \
	                                            (go . t))))" \
	    -f ob-go-test-runall
