(bb-package spaceline
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-minor-modes-separator "")
  (spaceline-spacemacs-theme)
  (with-eval-after-load 'helm
    (spaceline-helm-mode)))
