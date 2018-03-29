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
  :config
  (global-auto-revert-mode))

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
