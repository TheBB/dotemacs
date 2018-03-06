(setq package-user-dir (concat bb-cfg-dir "elpa/")
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/"))

      bb-packages
      `(
	(evil :location ,(concat bb-cfg-dir "third-party/evil"))
	monokai-theme
	(emacs :location internal)
	))
