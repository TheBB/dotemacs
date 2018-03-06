(bb-package emacs
  :boot
  (setq backup-directory-alist `(("." . ,(bb-dir ".cache/backups")))
	backup-by-copying t
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	create-lockfiles nil))
