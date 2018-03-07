(setq package-user-dir (concat bb-cfg-dir "elpa/")
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/"))

      bb-packages
      `(
        general
	monokai-theme
        (spaceline :location ,(concat bb-cfg-dir "third-party/spaceline"))

	(emacs :location internal)
	(evil :location ,(concat bb-cfg-dir "third-party/evil"))
        helm
        page-break-lines

        dash
        s
        powerline
	))
