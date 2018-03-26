-include lib/borg/borg.mk

dirs:
	mkdir -p elpa
	mkdir -p .cache/backups

update: dirs
	rm -rf elpa.bak
	cp -R elpa elpa.bak
	rm -rf elpa/*
	$(EMACS) -f bb-update
