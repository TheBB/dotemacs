(bb-package smartparens
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (with-eval-after-load 'smartparens
    (require 'smartparens-config)
    (diminish 'smartparens-mode "s"))
  (bb-leader "ts" 'smartparens-mode))
