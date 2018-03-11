(bb-package emacs-lisp-mode

  :post-init general
  (bb-mm-leader emacs-lisp-mode
    "cp" eval-last-sexp
    "cf" eval-defun
    "cb" eval-buffer)

  :post-init evil-collection
  (with-eval-after-load 'elisp-mode
    (require 'evil-collection-elisp-mode)
    (evil-collection-elisp-mode-setup)
    (advice-add 'eval-last-sexp :around 'evil-collection-elisp-mode-last-sexp))

  :post-init company
  (bb-company emacs-lisp-mode company-capf))
