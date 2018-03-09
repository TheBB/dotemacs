(setq package-user-dir (concat bb-cfg-dir "elpa/")
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))

      bb-packages
      `(
        general
        hydra
        monokai-theme
        diminish
        (spaceline :location ,(concat bb-cfg-dir "third-party/spaceline"))
        exec-path-from-shell

        (emacs :location internal)

        company
        company-childframe
        (eldoc :location internal)
        (evil :location ,(concat bb-cfg-dir "third-party/evil"))
        evil-collection
        helm
        helm-projectile
        magit
        evil-magit
        page-break-lines
        projectile
        ws-butler

        (emacs-lisp-mode :location internal)

        ;; Packages needed as dependencies
        dash
        powerline
        s
        undo-tree
        ))
