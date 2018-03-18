(bb-package yasnippet
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (with-eval-after-load 'yasnippet
    (diminish 'yas-minor-mode "y")
    (yas-reload-all))

  :post-init company
  (push 'company-yasnippet bb-company-global-backends)

  :post-init evil
  (define-key evil-insert-state-map (kbd "C-SPC") 'yas-expand))
