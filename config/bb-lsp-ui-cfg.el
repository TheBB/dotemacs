(defun bb-lsp-enable-ui ()
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t))

(defun bb-lsp-ui-flycheck-enable (_)
  (setq-local flycheck-checker 'lsp-ui)
  (lsp-ui-flycheck-add-mode major-mode)
  (add-to-list 'flycheck-checkers 'lsp-ui))
