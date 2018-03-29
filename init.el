;;; -*- lexical-binding: t -*-



;; Initialize borg and set up load path

(setq user-init-file (or load-file-name buffer-file-name)
      user-emacs-directory (file-name-directory user-init-file)

      bb-cfg-dir user-emacs-directory
      custom-file (concat bb-cfg-dir "custom.el"))

(push (concat bb-cfg-dir "lib") load-path)

;; The load path must be set when compiling to get access to macros
(eval-when-compile
  (push (concat bb-cfg-dir "config") load-path)
  (require 'bb-compile)
  (load (concat bb-cfg-dir "config.el")))

(push (concat bb-cfg-dir "lib/borg") load-path)
(require 'borg)
(borg-initialize)



;; Function, variable, and macro definitions

(require 'bb-defs)



;; Packages that should be enabled early

(use-package general
  :config
  (setq general-override-states '(normal visual motion))
  (general-override-mode))

(use-package no-littering)

(use-package popwin
  :config
  (setq popwin:special-display-config nil)
  (popwin-mode))



;; Modeline

(use-package spaceline-segments
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
	spaceline-minor-modes-separator "")
  :config
  (spaceline-generate
    (((persp-name workspace-number window-number) :fallback evil-state :face highlight-face :priority 100)
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
      indent-tabs-mode nil
      load-prefer-newer t
      read-quoted-char-radix 16
      require-final-newline t
      ring-bell-function 'ignore
      scroll-conservatively 101
      vc-follow-symlinks t
      x-wait-for-event-timeout 0.05)

(fset 'yes-or-no-p 'y-or-n-p)
(fset 'startup-echo-area-message (lambda () ""))

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

(bb-leader
  "<tab>" 'bb-alternate-buffer
  ";" 'eval-expression
  "bd" 'bb-kill-buffer
  "fd" 'bb-kill-buffer-file
  "fs" 'save-buffer
  "fy" 'bb-show-and-copy-filename
  "w" 'hydra-windows/body)

(push '(buffer-predicate . bb-useful-buffer-p) default-frame-alist)



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
  (define-key evil-motion-state-map (kbd "<left>") 'windmove-left)
  (define-key evil-motion-state-map (kbd "<down>") 'windmove-down)
  (define-key evil-motion-state-map (kbd "<up>") 'windmove-up)
  (define-key evil-motion-state-map (kbd "<right>") 'windmove-right)
  (define-key evil-motion-state-map (kbd "gd") 'xref-find-definitions)
  (define-key evil-visual-state-map (kbd "J") (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map (kbd "K") (concat ":m '<-2" (kbd "RET") "gv=gv"))

  ;; Unimpaired
  (define-key evil-motion-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-motion-state-map (kbd "] b") 'next-buffer)
  (define-key evil-normal-state-map (kbd "[ SPC") 'bb-insert-line-above)
  (define-key evil-normal-state-map (kbd "] SPC") 'bb-insert-line-below)
  (define-key evil-normal-state-map (kbd "[ s") 'bb-insert-spaces-before)
  (define-key evil-normal-state-map (kbd "] s") 'bb-insert-spaces-after))

(use-package evil-args
  :defer t
  :init
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-collection-integration
  :after evil)

(use-package evil-embrace
  :after evil-surround
  :config
  (evil-embrace-enable-evil-surround-integration))

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
  :diminish evil-smartparens-mode)

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region))



;; LSP and Co.

(use-package lsp-mode
  :commands (lsp-make-traverser)
  :diminish (lsp-mode . "l")
  :defer t
  :init
  (eval-when-compile (require 'lsp-mode))
  (setq lsp-highlight-symbol-at-point nil)
  (bb-leader "tl" 'lsp-mode)
  (put 'lsp-define-stdio-client 'lisp-indent-function 2))

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



;; Miscellaneous

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :diminish (smartparens-mode . "s")
  :init
  (setq sp-highlight-pair-overlay nil
	sp-highlight-wrap-overlay nil
	sp-highlight-wrap-tag-overlay nil)
  (bb-leader "ts" 'smartparens-mode))

(use-package smartparens-config
  :after smartparens)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-tree-enable-undo-in-region nil)
  (bb-leader "au" 'undo-tree-visualize)
  (bb-popwin undo-tree-visualizer-mode :width 60 :position right)
  :config
  (global-undo-tree-mode))



;; The configuration stage runs code from all bb-PKG-cfg.el files
;; Generally intended for defuns and defvars
(bb-collect-cfg)

;; The boot stage runs code that needs to happen as soon as Emacs boots
;; Maybe this should go in early-init?
(bb-stage boot)

;; Code injected from other packages
(bb-stage pre-init)

;; Actual configuration code
(bb-stage init)

;; Code injected from other packages
(bb-stage post-init)

;; Finally load customizations, if any
(load custom-file)
