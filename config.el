(setq package-user-dir (bb-dir "elpa/")
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))

      bb-packages
      `(
        general
        hydra
        monokai-theme
        diminish
        (spaceline :location ,(bb-dir "third-party/spaceline"))
        exec-path-from-shell

        (emacs :location internal)

        company
        company-childframe
        (eldoc :location internal)
        (evil :location ,(bb-dir "third-party/evil"))
        evil-args
        evil-collection
        evil-embrace
        (evil-little-word :location ,(bb-dir "third-party/evil-little-word"))
        evil-surround
        (evil-unimpaired :location internal)
        flycheck
        helm
        helm-ag
        helm-projectile
        helm-xref
        lsp-mode
        company-lsp
        lsp-ui
        magit
        evil-magit
        page-break-lines
        projectile
        ws-butler

        (emacs-lisp-mode :location internal)
        cython-mode
        python
        pyvenv
        (text-mode :location internal)

        ;; Packages needed as dependencies
        dash
        powerline
        s
        undo-tree
        ))
