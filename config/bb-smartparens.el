(bb-package smartparens
  :init
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (with-eval-after-load 'smartparens
    (require 'smartparens-config)
    (diminish 'smartparens-mode "s"))
  (bb-leader "ts" 'smartparens-mode))
