EMACS = emacs --batch -l early-init.el -l config.el -l bb-compile

compile:
	$(EMACS) -f bb-compile

update:
	mkdir -p elpa
	rm -rf elpa.bak
	cp -R elpa elpa.bak
	rm -rf elpa/*
	$(EMACS) -f bb-update
