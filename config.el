(setq bb-packages
      `(
        ;; Planning to migrate this to a borg collective eventually
        (borg :location internal)

        ;; Packages that should be initialized early
        (general :location internal)
        (hydra :location internal)
        (monokai-theme :location internal)
        (diminish :location diminish)
        (spaceline :location internal)
        (exec-path-from-shell :location internal)

        ;; General setup involving Emacs built-in stuff
        (emacs :location internal)

        ;; Company and friends
        (company :location internal)
        (company-childframe :location internal)

        ;; Evil and friends
        (evil :location internal)
        (evil-args :location internal)
        (evil-collection :location internal)
        (evil-embrace :location internal)
        (evil-little-word :location internal)
        (evil-nerd-commenter :location internal)
        (evil-numbers :location internal)
        (evil-surround :location internal)
        (evil-unimpaired :location internal)

        ;; Helm and friends
        (helm :location internal)
        (helm-ag :location internal)
        (helm-projectile :location internal)
        (helm-swoop :location internal)
        (helm-xref :location internal)

        ;; Magit and friends
        (magit :location internal)
        (evil-magit :location internal)
        (magithub :location internal)

        ;; LSP and friends
        (lsp-mode :location internal)
        (company-lsp :location internal)
        (cquery :location internal)
        (lsp-ui :location internal)

        ;; Structured editing
        (smartparens :location internal)
        (evil-smartparens :location internal)

        ;; Snippets
        (yasnippet :location internal)
        (yasnippet-snippets :location internal)

        ;; Miscellaneous
        (eldoc :location internal)
        (flycheck :location internal)
        (highlight-operators :location internal)
        (highlight-numbers :location internal)
        (structured-editing :location internal)
        (page-break-lines :location internal)
        (popwin :location internal)
        (projectile :location internal)
        (ws-butler :location internal)

        ;; Major modes
        (c-cpp :location internal)
        (cmake-mode :location internal)
        (cython-mode :location internal)
        (emacs-lisp-mode :location internal)
        (macrostep :location internal)
        (python :location internal)
        (pyvenv :location internal)
        (text-mode :location internal)
        (yaml-mode :location internal)

        ;; Packages needed as dependencies
        (dash :location internal)
        (powerline :location internal)
        (s :location internal)
        (undo-tree :location internal)
        ))
