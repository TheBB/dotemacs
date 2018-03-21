(bb-package helm-swoop
  :init
  (setq helm-swoop-split-with-multiple-windows t
        helm-swoop-pre-input-function (lambda () ""))
  (bb-leader "ss" 'bb-helm-swoop))
