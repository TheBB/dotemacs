(bb-package evil
  :init
  (setq-default
   evil-normal-state-cursor '("DarkGoldenrod2" box)
   evil-insert-state-cursor '("chartreuse3" (bar . 2))
   evil-emacs-state-cursor '("SkyBlue2" box)
   evil-replace-state-cursor '("chocolate" (hbar . 2))
   evil-visual-state-cursor '("gray" (hbar . 2))
   evil-motion-state-cursor '("plum3" box))
  (setq evil-want-integration nil
        evil-want-C-u-scroll t)

  (require 'evil)
  (evil-mode)

  (define-key evil-motion-state-map (kbd "<left>") 'windmove-left)
  (define-key evil-motion-state-map (kbd "<down>") 'windmove-down)
  (define-key evil-motion-state-map (kbd "<up>") 'windmove-up)
  (define-key evil-motion-state-map (kbd "<right>") 'windmove-right)
  (define-key evil-motion-state-map (kbd "gd") 'xref-find-definitions)

  (define-key evil-visual-state-map (kbd "J") (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map (kbd "K") (concat ":m '<-2" (kbd "RET") "gv=gv")))
