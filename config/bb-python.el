(bb-package python

  :post-init lsp-mode
  (lsp-define-stdio-client lsp-python "python"
    (lsp-make-traverser (lambda (dir)
                          (directory-files dir nil "\\(setup\\)\\.py")))
    '("pyls"))

  ;; Don't enable in derived modes (like cython-mode)
  (defun bb-lsp-python-enable (func &rest args)
    (when (eq 'python-mode major-mode) (apply func args)))
  (advice-add 'lsp-python-enable :around 'bb-lsp-python-enable)
  (add-hook 'python-mode-hook 'lsp-python-enable)

  :post-init smartparens
  (with-eval-after-load 'smartparens
    (bb-apply-newline-indent (python-mode) "{" "[" "(")))
