;;; init.el --- Personal configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Eivind Fonn

;; This file is not part of GNU Emacs.

;;; License:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; General init of Emacs.

;;; Code:



;; Initialize borg and set up load path

(setq user-init-file (or load-file-name buffer-file-name)
      user-emacs-directory (file-name-directory user-init-file)
      custom-file (concat user-emacs-directory "custom.el")

      ;; Enable Keyboardio Model 01 mode
      keyboardiop (string= (system-name) "cauchy")
      bb-left (kbd (if keyboardiop "<left>" "C-h"))
      bb-down (kbd (if keyboardiop "<down>" "C-j"))
      bb-up (kbd (if keyboardiop "<up>" "C-k"))
      bb-right (kbd (if keyboardiop "<right>" "C-l")))

(push (concat user-emacs-directory "lib") load-path)
(push (concat user-emacs-directory "lib/borg") load-path)
(require 'borg)
(borg-initialize)



;; Local settings, if any

(let ((filename (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p filename)
    (load-file filename)))



;; Function, variable, and macro definitions

(require 'bb-defs)
(require 'bb-hooks)



;; Packages that should be enabled early

(use-package general
  :config
  (setq general-override-states '(normal visual motion))
  (general-override-mode))

(use-package no-littering)



;; Theme

(load-theme 'monokai 'noconfirm)
(require 'color)

(bb-after-display
  (set-face-attribute 'default nil :font "Iosevka Expanded Bold" :height 100))
(set-face-attribute 'region nil :background (color-lighten-name monokai-gray 10))
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-string-face nil :slant 'italic)
(set-face-attribute 'font-lock-doc-face nil :slant 'italic :foreground "#75715e")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#ff4185" :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil :foreground "#ffabd6" :weight 'bold)

(set-face-attribute 'header-line nil :box '(:color "#555555"))
(set-face-attribute 'mode-line nil
  :box '(:color "#999999" :line-width 1 :style released-button))
(set-face-attribute 'mode-line-inactive nil
  :box '(:color "#666666" :line-width 1 :style released-button))



;; Modeline

(use-package spaceline-segments
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
	spaceline-minor-modes-separator ""
        spaceline-purpose-hide-if-not-dedicated t)
  :config
  (spaceline-generate
    (((evil-state workspace-number window-number) :face highlight-face :priority 100)
     (anzu :priority 95)
     (auto-compile)
     ((buffer-modified buffer-size buffer-id remote-host) :priority 98)
     (major-mode :priority 79)
     (process :when active)
     ((flycheck-error flycheck-warning flycheck-info) :when active :priority 89)
     (minor-modes :when active :priority 9)
     (erc-track :when active)
     (version-control :when active :priority 78))
    (which-function
     (python-pyvenv :fallback python-pyenv)
     (purpose :priority 94)
     (selection-info :priority 95)
     input-method
     ((buffer-encoding-abbrev
       point-position
       line-column)
      :separator " | "
      :priority 96)
     (global :when active)
     (buffer-position :priority 99)
     (hud :priority 99)))
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package spaceline-config
  :after helm
  :config
  (spaceline-helm-mode))

(use-package powerline
  :defer t
  :config
  (set-face-attribute 'powerline-active1 nil
    :box '(:color "#999999" :line-width 1 :style released-button)
    :background "#5a5a5a")
  (set-face-attribute 'powerline-active2 nil
    :box '(:color "#999999" :line-width 1 :style released-button))
  (set-face-attribute 'powerline-inactive1 nil
    :box '(:color "#666666" :line-width 1 :style released-button))
  (set-face-attribute 'powerline-inactive2 nil
    :box '(:color "#666666" :line-width 1 :style released-button)))



;; General Emacs settings (built-ins, etc.)

(setq user-init-file (or load-file-name buffer-file-name)
      user-emacs-directory (file-name-directory user-init-file)

      inhibit-startup-buffer-menu t
      inhibit-startup-screen t
      initial-buffer-choice t
      initial-scratch-message ""

      auto-save-list-file-prefix nil
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      create-lockfiles nil

      auto-revert-check-vc-info t
      load-prefer-newer t
      read-quoted-char-radix 16
      require-final-newline t
      ring-bell-function 'ignore
      scroll-conservatively 101
      vc-follow-symlinks t
      x-wait-for-event-timeout 0.05)

(setq-default fill-column 100
              indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)
(fset 'startup-echo-area-message (lambda () ""))
(put 'set-face-attribute 'lisp-indent-function 2)
(push '(buffer-predicate . bb-useful-buffer-p) default-frame-alist)
(bb-popwin special-mode)

(use-package abbrev
  :defer t
  :diminish abbrev-mode)

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode))

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package help-mode
  :defer t
  :init
  (bb-popwin help-mode))

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package simple
  :defer t
  :diminish auto-fill-function)

(use-package smerge-mode
  :defer t
  :diminish (smerge-mode . "[sm]"))

(use-package uniquify)

(use-package winner
  :config
  (winner-mode))

(use-package whitespace
  :defer t
  :diminish (whitespace-mode . "w")
  :init
  (bb-leader "tw" 'whitespace-mode))

(bb-leader
  "<tab>" 'bb-alternate-buffer
  ";" 'eval-expression
  "bd" 'bb-kill-buffer
  "fd" 'bb-kill-buffer-file
  "fs" 'save-buffer
  "fy" 'bb-show-and-copy-filename
  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdF" 'describe-face
  "hdk" 'describe-key
  "hdv" 'describe-variable
  "u" 'universal-argument
  "w" 'hydra-windows/body)



;; Evil and Co.

(use-package evil
  :init
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box)
	evil-insert-state-cursor '("chartreuse3" (bar . 2))
	evil-emacs-state-cursor '("SkyBlue2" box)
	evil-replace-state-cursor '("chocolate" (hbar . 2))
	evil-visual-state-cursor '("gray" (hbar . 2))
	evil-motion-state-cursor '("plum3" box)
	evil-want-integration nil
	evil-want-C-u-scroll t)
  :config
  (evil-mode)
  (add-hook 'evil-insert-state-exit-hook 'bb-evil-insert-state-exit)

  ;; Miscellaneous keybindings
  (define-key evil-motion-state-map (kbd "<left>") 'windmove-left)
  (define-key evil-motion-state-map (kbd "<down>") 'windmove-down)
  (define-key evil-motion-state-map (kbd "<up>") 'windmove-up)
  (define-key evil-motion-state-map (kbd "<right>") 'windmove-right)
  (define-key evil-motion-state-map (kbd "C-j") 'eyebrowse-next-window-config)
  (define-key evil-motion-state-map (kbd "C-k") 'eyebrowse-prev-window-config)

  (define-key evil-motion-state-map (kbd "gd") 'xref-find-definitions)
  (define-key evil-visual-state-map (kbd "J") (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map (kbd "K") (concat ":m '<-2" (kbd "RET") "gv=gv"))

  ;; Unimpaired
  (define-key evil-motion-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-motion-state-map (kbd "] b") 'next-buffer)
  (define-key evil-normal-state-map (kbd "[ SPC") 'bb-insert-line-above)
  (define-key evil-normal-state-map (kbd "] SPC") 'bb-insert-line-below)
  (define-key evil-normal-state-map (kbd "[ s") 'bb-insert-spaces-before)
  (define-key evil-normal-state-map (kbd "] s") 'bb-insert-spaces-after)

  ;; Other
  (evil-set-command-property 'xref-find-definitions :jump t))

(use-package evil-args
  :defer t
  :init
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-collection-integration
  :after evil)

(use-package evil-collection-elisp-mode
  :after elisp-mode
  :config
  (evil-collection-elisp-mode-setup)
  (advice-add 'eval-last-sexp :around 'evil-collection-elisp-mode-last-sexp))

(use-package evil-embrace
  :after evil-surround
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-escape
  :diminish evil-escape-mode
  :config
  (unless keyboardiop
    (evil-escape-mode))
  (define-key minibuffer-local-map [escape] 'evil-escape)
  (define-key minibuffer-local-ns-map [escape] 'evil-escape)
  (define-key minibuffer-local-completion-map [escape] 'evil-escape)
  (define-key minibuffer-local-must-match-map [escape] 'evil-escape)
  (define-key minibuffer-local-isearch-map [escape] 'evil-escape))

(use-package evil-little-word
  :after evil)

(use-package evil-nerd-commenter
  :defer t
  :init
  (bb-leader
    "cl" 'evilnc-comment-or-uncomment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cy" 'evilnc-copy-and-comment-lines))

(use-package evil-numbers
  :defer t
  :init
  (define-key evil-normal-state-map (kbd "+") 'hydra-numbers/evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "_") 'hydra-numbers/evil-numbers/dec-at-pt))

(use-package evil-smartparens
  :hook (smartparens-enabled . evil-smartparens-mode)
  :diminish evil-smartparens-mode
  :config
  (bb-advise after evil-sp--add-bindings ()
    (evil-define-key 'visual evil-smartparens-mode-map (kbd "o") nil)))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region))



;; Company and Co.

(use-package company
  :defer t
  :diminish (company-mode . "c")
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-require-match nil
        company-tooltip-align-annotations t
        company-tooltip-minimum-width 60)
  (bb-leader "tc" 'company-mode)
  :config
  (define-key company-active-map bb-right 'company-complete-selection)
  (define-key company-active-map bb-down 'company-select-next-or-abort)
  (define-key company-active-map bb-up 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "C-w") nil)
  (set-face-attribute 'company-tooltip-selection nil
    :background monokai-comments :foreground monokai-emphasis)
  (set-face-attribute 'company-tooltip-common-selection nil
    :foreground monokai-blue :background monokai-comments)
  (set-face-attribute 'company-tooltip-annotation-selection nil
    :background monokai-comments))



;; Flycheck and Co.

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :diminish (flycheck-mode . "f")
  :init
  (setq-default flycheck-check-syntax-automatically nil)
  (bb-leader
    "tf" 'flycheck-mode
    "el" 'flycheck-list-errors
    "eb" 'flycheck-buffer
    "ec" 'flycheck-clear)
  (bb-popwin flycheck-error-list-mode)
  :config
  (aset flycheck-error-list-format 5 '("Message" 0 t)))



;; Helm and Co.

(use-package helm
  :diminish helm-mode
  :init
  (setq helm-display-function 'bb-helm-display-child-frame
        helm-display-buffer-reuse-frame t
        helm-display-buffer-width 120
        helm-display-buffer-height 25)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  (bb-leader
    "SPC" 'helm-M-x
    "bb" 'helm-mini
    "ff" 'helm-find-files
    "fl" 'helm-locate-library
    "hi" 'bb-find-init
    "ji" 'helm-imenu
    "rl" 'helm-resume)
  (push "\\*helm.+\\*" bb-useless-buffers-regexp)
  :config
  (helm-mode)
  (helm-autoresize-mode)
  (define-key helm-map bb-right 'helm-maybe-exit-minibuffer)
  (define-key helm-map bb-down 'helm-next-line)
  (define-key helm-map bb-up 'helm-previous-line)
  (set-face-attribute 'helm-prefarg nil :foreground "PaleGreen"))

(use-package helm-ag
  :defer t
  :init
  (bb-leader "/" 'bb-helm-ag-project)
  :config
  (define-key helm-ag-map bb-right nil)
  (define-key helm-ag-map bb-left 'helm-ag--up-one-level)
  (when keyboardiop
    (define-key helm-ag-map (kbd "C-j") 'helm-ag--next-file)
    (define-key helm-ag-map (kbd "C-k") 'helm-ag--previous-file)))

(use-package helm-files
  :defer t
  :config
  (define-key helm-find-files-map bb-right 'helm-ff-RET)
  (define-key helm-find-files-map bb-left 'helm-find-files-up-one-level)
  (advice-add 'helm-ff-filter-candidate-one-by-one
	      :around 'bb-helm-ff-filter-candidate-one-by-one)
  (advice-add 'helm-find-files-up-one-level
	      :around 'bb-helm-find-files-up-one-level))

(use-package helm-imenu
  :defer t
  :config
  (define-key helm-imenu-map bb-right 'helm-maybe-exit-minibuffer))

(use-package helm-projectile
  :commands (helm-projectile
             helm-projectile-find-dir
             helm-projectile-find-file
             helm-projectile-switch-project
             helm-projectile-switch-to-buffer)
  :init
  (setq projectile-switch-project-action 'helm-projectile)
  (bb-leader
    "pb" 'helm-projectile-switch-to-buffer
    "pd" 'helm-projectile-find-dir
    "pf" 'helm-projectile-find-file
    "ph" 'helm-projectile
    "pp" 'helm-projectile-switch-project)
  :config
  (define-key helm-projectile-find-file-map bb-right 'helm-maybe-exit-minibuffer))

(use-package helm-swoop
  :init
  (setq helm-swoop-split-with-multiple-windows t
        helm-swoop-pre-input-function (lambda () ""))
  (bb-leader "ss" 'bb-helm-swoop))

(use-package helm-xref
  :after xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))



;; IRC and Co.

(use-package erc
  :defer t
  :init
  (setq erc-timestamp-format-let "\n%A %B %e, %Y\n\n"
        erc-timestamp-format-right "%H:%M"
        erc-timestamp-right-column 80
        erc-prompt-for-nickserv-password nil
        erc-image-inline-rescale 200
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
        erc-track-position-in-mode-line nil)
  (bb-define-display "irc" "ai"
    :layout "irc"
    :startup (erc :server "efonn.no"
                  :port 1025
                  :nick "TheBB"
                  :password (format "TheBB/freenode:%s" bb-znc-pwd)))
  (bb-mm-leader erc-mode "qs" 'erc-quit-server)
  (evil-set-initial-state 'erc-mode 'normal)
  (add-hook 'erc-mode-hook 'bb-erc)
  :config
  (setq erc-modules (remove 'track erc-modules))
  (erc-update-modules))



;; LSP and Co.

(use-package lsp-mode
  :commands (lsp-make-traverser)
  :diminish (lsp-mode . "l")
  :defer t
  :init
  ;; We use `lsp-define-stdio-client' at compile-time
  (eval-when-compile (require 'lsp-mode))
  (put 'lsp-define-stdio-client 'lisp-indent-function 2)

  (setq lsp-highlight-symbol-at-point nil)
  (bb-leader "tl" 'lsp-mode)
  (define-key evil-insert-state-map (kbd "C-l") 'company-complete)
  (bb-company lsp-mode company-lsp))

(use-package lsp-methods
  :defer t
  :config
  (set-face-attribute 'lsp-face-highlight-textual nil
    :background monokai-highlight-line))

(use-package lsp-ui
  :hook (lsp-mode . bb-lsp-enable-ui))

(use-package lsp-ui-doc :commands lsp-ui-doc-enable)
(use-package lsp-ui-flycheck :commands (lsp-ui-flycheck-enable lsp-ui-flycheck-add-mode))
(use-package lsp-ui-imenu :commands lsp-ui-imenu-enable)
(use-package lsp-ui-sideline :commands lsp-ui-sideline-enable)



;; Magit and Co.

(use-package magit
  :defer t
  :init
  (bb-leader "gs" 'magit-status)
  (push "magit.*" bb-useless-buffers-regexp))

(use-package evil-magit
  :after magit)

(use-package magithub
  :after magit
  :init
  (setq magithub-clone-default-directory "~/repos")
  :config
  (magithub-feature-autoinject 'all))

(use-package with-editor
  :defer t
  :diminish with-editor-mode)



;; LaTeX and Co.

(use-package bibtex
  :defer t
  :init
  (setq bibtex-align-at-equal-sign t)
  (bb-mm-leader bibtex-mode "==" 'bibtex-fill-entry))

(use-package company-reftex
  :defer t
  :init
  (setq company-reftex-max-annotation-length 100))

(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  :diminish reftex-mode)

(use-package tex-site
  ;; Not deferred, since tex-site.el is essentially an autoloads file.
  :init
  (setq tex-fontify-script nil
	font-latex-fontify-script nil
        TeX-parse-self nil)
  (bb-company LaTeX-mode
    company-reftex-labels company-reftex-citations
    company-auctex-macros company-auctex-environments))



;; Programming languages and other major modes

(use-package cc-mode
  :defer t
  :hook ((c-mode c++-mode) . lsp-cquery-enable))

(use-package cc-styles
  :defer t
  :config
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
  (push '(other . "personal") c-default-style))

(use-package cmake-mode
  :defer t
  :init
  (bb-company cmake-mode company-cmake))

(use-package elisp-mode
  :defer t
  :init
  (bb-mm-leader emacs-lisp-mode
    "cs" 'eval-last-sexp
    "cf" 'eval-defun
    "cb" 'eval-buffer
    "l" 'hydra-structured-editing-lisp/body)
  (bb-company emacs-lisp-mode company-capf)
  (add-hook 'emacs-lisp-mode-hook 'bb-elisp))

(use-package lisp-mode
  :defer t
  :init
  (bb-mm-leader lisp-mode "l" 'hydra-structured-editing-lisp/body))

(use-package python
  :defer t
  :hook (python-mode . lsp-python-enable)
  :init
  ;; The "official" client wrongly considers __init__.py to be a project root
  (lsp-define-stdio-client lsp-python "python"
    (lsp-make-traverser (lambda (dir)
                          (directory-files dir nil "\\`setup\\.py\\'")))
    '("pyls"))
  ;; Don't enable LSP in derived modes, like cython-mode, which are not Python
  (bb-adv-only-in-modes lsp-python-enable python-mode))

(use-package pyvenv
  :defer t
  :init
  (bb-mm-leader python-mode
    "va" 'pyvenv-workon
    "vd" 'pyvenv-deactivate)
  (add-hook 'pyvenv-post-activate-hooks 'lsp-restart-workspace)
  (add-hook 'pyvenv-post-deactivate-hooks 'lsp-restart-workspace))

(use-package text-mode
  :hook (text-mode . auto-fill-mode))



;; Miscellaneous

(use-package ace-window
  :defer t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package eyebrowse
  :init
  (setq eyebrowse-new-workspace t
        eyebrowse-wrap-around t)
  :config
  (eyebrowse-mode))

(use-package highlight-operators
  :hook (prog-mode . highlight-operators-mode)
  :init
  (bb-adv-except-derived-modes highlight-operators-mode
    lisp-mode scheme-mode emacs-lisp-mode python-mode)
  :config
  (set-face-attribute 'highlight-operators-face nil
    :inherit 'font-lock-keyword-face))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package lispy
  :diminish (lispy-mode . "L")
  :hook ((lisp-mode scheme-mode emacs-lisp-mode) . lispy-mode))

(use-package lispyville
  :diminish lispyville-mode
  :hook (lispy-mode . lispyville-mode))

(use-package macrostep
  :defer t
  :init
  (bb-mm-leader emacs-lisp-mode "cm" 'hydra-macrostep/body))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :config
  (setq page-break-lines-mode '(prog-mode))
  (global-page-break-lines-mode))

(use-package projectile
  :diminish projectile-mode
  :init
  (bb-leader "ga" 'projectile-find-other-file)
  :config
  (projectile-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook ((prog-mode LaTeX-mode) . smartparens-mode)
  :diminish (smartparens-mode . "s")
  :init
  (setq sp-highlight-pair-overlay nil
	sp-highlight-wrap-overlay nil
	sp-highlight-wrap-tag-overlay nil)
  (bb-leader "ts" 'smartparens-mode)
  :config
  (bb-adv-except-derived-modes smartparens-mode
    lisp-mode scheme-mode emacs-lisp-mode)
  (sp-local-pair '(c-mode c++-mode) "'" nil :post-handlers '(:rem sp-escape-quotes-after-insert))
  (bb-apply-newline-indent (c-mode c++-mode python-mode) "{" "[" "("))

(use-package smartparens-config
  :after smartparens)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-tree-enable-undo-in-region nil)
  (bb-leader "au" 'undo-tree-visualize)
  :config
  (global-undo-tree-mode))

(use-package window-purpose
  :diminish purpose-mode
  :init
  (push "\\*pu-dummy-" bb-useless-buffers-regexp)
  (push "\\*pu-dummy-" helm-boring-buffer-regexp-list)
  :config
  (setq purpose-use-default-configuration nil
        purpose-user-regexp-purposes
        '(("magit" . general))
        purpose-user-name-purposes
        '(("#emacs" . erc-emacs)
          ("#evil-mode" . erc-evil-mode)
          ("#emacs-beginners" . erc-emacs-beginners))
        purpose-user-mode-purposes
        '((erc-mode . erc)
          (fundamental-mode . general)))
  (purpose-compile-user-configuration)
  (purpose-mode))

(use-package window-purpose-x
  :config
  (purpose-x-popwin-setup)
  (purpose-x-popwin-update-conf))

(use-package ws-butler
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode))

(use-package yasnippet
  :hook ((prog-mode LaTeX-mode) . yas-minor-mode)
  :diminish (yas-minor-mode . "y")
  :init
  (push 'company-yasnippet bb-company-global-backends)
  (define-key evil-insert-state-map (kbd "C-SPC") 'yas-expand)
  :config
  (yas-reload-all))



;; Finally load customizations, if any

(load custom-file)


;;; init.el ends here
