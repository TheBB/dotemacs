(bb-package c-cpp
  :init
  (add-hook 'c-mode-hook 'lsp-cquery-enable)
  (add-hook 'c++-mode-hook 'lsp-cquery-enable)

  (with-eval-after-load 'cc-styles
    (c-add-style "personal"
                 '((indent-tabs-mode . nil)
                   (c-basic-offset . 4)
                   (c-offsets-alist
                    (arglist-close . 0)
                    (inextern-lang . 0)
                    (inline-open . 0)
                    (innamespace . 0)
                    (statement-cont . c-lineup-assignments)
                    (substatement-open . 0))))
    (push '(other . "personal") c-default-style)))
