(bb-package helm
  :init
  (setq helm-display-function 'bb-helm-display-child-frame
        helm-display-buffer-reuse-frame t
        helm-display-buffer-width 80)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  (with-eval-after-load 'helm-files
    (define-key helm-find-files-map (kbd "<right>") 'helm-ff-RET)
    (advice-add 'helm-ff-filter-candidate-one-by-one
                :around 'bb-helm-ff-filter-candidate-one-by-one)
    (advice-add 'helm-find-files-up-one-level
                :around 'bb-helm-find-files-up-one-level))
  :post-init general
  (bb-leader "ff" 'helm-find-files
             "hh" 'bb-helm-config
             ))
