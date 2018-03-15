(bb-package evil-smartparens
  :post-init evil
  (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode)
  (with-eval-after-load 'evil-smartparens
    (diminish 'evil-smartparens-mode)))
