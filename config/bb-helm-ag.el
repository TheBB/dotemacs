(bb-package helm-ag
  :init
  (bb-leader "/" 'bb-helm-ag-project)
  (with-eval-after-load 'helm-ag
    (define-key helm-ag-map (kbd "<right>") nil)
    (define-key helm-ag-map (kbd "<left>") 'helm-ag--up-one-level)
    (define-key helm-ag-map (kbd "C-j") 'helm-ag--next-file)
    (define-key helm-ag-map (kbd "C-k") 'helm-ag--previous-file)))
