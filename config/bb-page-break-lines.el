(bb-package page-break-lines
  :init
  (setq page-break-lines-modes '(prog-mode))
  (global-page-break-lines-mode)
  (diminish 'page-break-lines-mode))
