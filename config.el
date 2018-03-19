(setq package-user-dir (bb-dir "elpa/")
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))

      bb-packages
      `(
        ;; Packages that should be initialized early
        general
        hydra
        monokai-theme
        diminish
        (spaceline :location ,(bb-dir "third-party/spaceline"))
        exec-path-from-shell

        ;; General setup involving Emacs built-in stuff
        (emacs :location internal)

        ;; Company and friends
        company
        company-childframe

        ;; Evil and friends
        (evil :location ,(bb-dir "third-party/evil"))
        evil-args
        evil-collection
        evil-embrace
        (evil-little-word :location ,(bb-dir "third-party/evil-little-word"))
        evil-nerd-commenter
        evil-numbers
        evil-surround
        (evil-unimpaired :location internal)

        ;; Helm and friends
        helm
        helm-ag
        helm-projectile
        helm-swoop
        helm-xref

        ;; Magit and friends
        magit
        evil-magit

        ;; LSP and friends
        lsp-mode
        company-lsp
        cquery
        lsp-ui

        ;; Structured editing
        smartparens
        evil-smartparens

        ;; Snippets
        yasnippet
        yasnippet-snippets

        ;; Miscellaneous
        (eldoc :location internal)
        (eshell :location internal)
        flycheck
        (structured-editing :location internal)
        page-break-lines
        popwin
        projectile
        ws-butler

        ;; Major modes
        (c-cpp :location internal)
        cmake-mode
        cython-mode
        (emacs-lisp-mode :location internal)
        python
        pyvenv
        (text-mode :location internal)

        ;; Packages needed as dependencies
        dash
        powerline
        s
        undo-tree
        ))
