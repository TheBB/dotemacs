(bb-package emacs
  :boot

  (put 'bb-autoload 'lisp-indent-function 1)
  (put 'bb-package 'lisp-indent-function 1)
  (put 'bb-leader 'lisp-indent-function 0)
  (put 'bb-mm-leader 'lisp-indent-function 1)

  ;; Miscellaneous
  (setq-default
   recentf-save-file (bb-dir ".cache/recentf")
   scroll-conservatively 101
   inhibit-startup-screen t
   vc-follow-symlinks t
   ring-bell-function 'ignore
   require-final-newline t
   indent-tabs-mode nil
   read-quoted-char-radix 16)
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Backups
  (setq-default
   auto-save-list-file-prefix nil
   backup-directory-alist `((".*" . ,(bb-dir ".cache/backups")))
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   create-lockfiles nil)

  :init
  (require 'uniquify)
  (global-auto-revert-mode)
  (diminish 'auto-revert-mode)

  (require 'hl-line)
  (global-hl-line-mode)

  (with-eval-after-load 'simple
    (diminish 'auto-fill-function))

  :post-init general
  (bb-leader
    "<tab>" 'bb-alternate-buffer
    "bd" 'bb-kill-buffer
    "fd" 'bb-kill-buffer-file
    "fs" 'save-buffer
    "hc" 'bb-find-config
    "w" 'hydra-windows/body)

  :post-init popwin
  (bb-popwin help-mode))
