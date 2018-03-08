(bb-package company
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil)
  (with-eval-after-load 'company
    (diminish 'company-mode)))
