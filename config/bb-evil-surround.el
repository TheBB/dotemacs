(bb-package evil-surround
  :post-init evil
  (require 'evil-surround)
  (global-evil-surround-mode)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region))
