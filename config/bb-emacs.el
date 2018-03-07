(bb-package emacs
  :boot

  (put 'bb-package 'lisp-indent-function 1)

  ;; Miscellaneous
  (setq-default
   vc-follow-symlinks t
   ring-bell-function 'ignore
   require-final-newline t
   indent-tabs-mode nil
   read-quoted-char-radix 16)

  ;; Backups
  (setq-default
   backup-directory-alist `(("." . ,(bb-dir ".cache/backups")))
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   create-lockfiles nil))
