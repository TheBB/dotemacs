(bb-package evil
  :init
  (setq-default
   evil-normal-state-cursor '("DarkGoldenrod2" box)
   evil-insert-state-cursor '("chartreuse3" (bar . 2))
   evil-emacs-state-cursor '("SkyBlue2" box)
   evil-replace-state-cursor '("chocolate" (hbar . 2))
   evil-visual-state-cursor '("gray" (hbar . 2))
   evil-motion-state-cursor '("plum3" box))
  (setq evil-want-integration nil)

  (require 'evil)
  (evil-mode)

  (define-key evil-motion-state-map (kbd "<left>") 'windmove-left)
  (define-key evil-motion-state-map (kbd "<down>") 'windmove-down)
  (define-key evil-motion-state-map (kbd "<up>") 'windmove-up)
  (define-key evil-motion-state-map (kbd "<right>") 'windmove-right))
