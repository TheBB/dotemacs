EMACS = emacs --batch -l early-init.el -l config.el -l bb-compile

dirs:
	mkdir -p elpa
	mkdir -p .cache/backups

compile: dirs
	$(EMACS) -f bb-compile

update: dirs
	rm -rf elpa.bak
	cp -R elpa elpa.bak
	rm -rf elpa/*
	$(EMACS) -f bb-update
