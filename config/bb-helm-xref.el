(bb-package helm-xref
  :init
  (with-eval-after-load 'xref
    (require 'helm-xref)
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs)))
