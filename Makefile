EMACS           ?= emacs
EMACS_ARGUMENTS ?= -Q

.PHONY: all help clean build build-init quick bootstrap

all: build

SILENCIO  = --load subr-x
SILENCIO += --eval "(put 'if-let   'byte-obsolete-info nil)"
SILENCIO += --eval "(put 'when-let 'byte-obsolete-info nil)"
SILENCIO += --eval "(fset 'original-message (symbol-function 'message))"
SILENCIO += --eval "(fset 'message\
(lambda (format &rest args)\
  (unless (equal format \"pcase-memoize: equal first branch, yet different\")\
    (apply 'original-message format args))))"

ELFILES = $(wildcard lisp/*.el)
ELCFILES = $(ELFILES:.el=.elc)

build build-init quick: $(ELCFILES)
	@$(MAKE) -f lib/borg/borg.mk $@

lisp: $(ELCFILES)

$(ELCFILES): %.elc: %.el
	@$(EMACS) $(EMACS_ARGUMENTS) \
	--batch -L lib/borg --load borg $(SILENCIO) \
	--funcall borg-initialize \
	-f batch-byte-compile $<

clean bootstrap:
	@$(MAKE) -f lib/borg/borg.mk $@

%: force
	@$(MAKE) -f lib/borg/borg.mk $@
force: ;
