(bb-package company
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 0
        company-require-match nil)
  (with-eval-after-load 'company
    (diminish 'company-mode)
    (define-key company-active-map (kbd "<right>") 'company-complete-selection)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "<return>") nil))

  :post-init lsp-mode
  (add-hook 'lsp-mode-hook 'bb-company-lsp))
