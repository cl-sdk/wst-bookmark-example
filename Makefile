ENV?=development

## NOTE: run sbcl loading the setting the current path
## on the `ql:*local-project-directories*`.
LISP?=sbcl --sysinit ./.sbclrc

LISPFLAGS=--non-interactive

run:
	$(LISP) $(LISPFLAGS) --eval "(ql:quickload :wst.example.bookmark-manager)"
