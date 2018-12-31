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


;;; Initialize borg and set up load path

(setq user-init-file (or load-file-name buffer-file-name)
      user-emacs-directory (file-name-directory user-init-file)
      custom-file (concat user-emacs-directory "custom.el")

      load-prefer-newer t

      ;; Enable Keyboardio Model 01 mode
      ;; Disable this for the moment on all computers
      ;; keyboardiop (string= (system-name) "cauchy")
      keyboardiop nil

      ;; Computer ID
      cauchyp (string= (system-name) "cauchy")

      bb-left (kbd (if keyboardiop "<left>" "C-h"))
      bb-down (kbd (if keyboardiop "<down>" "C-j"))
      bb-up (kbd (if keyboardiop "<up>" "C-k"))
      bb-right (kbd (if keyboardiop "<right>" "C-l"))

      ;; FIXME: Different fonts on different computers
      bb-font (if cauchyp
                  "Iosevka Expanded Term Bold"
                "Source Code Pro Semibold"))

(push (concat user-emacs-directory "lisp") load-path)
(push (concat user-emacs-directory "lib/borg") load-path)
(require 'borg)
(borg-initialize)


;;; Local settings, if any

(let ((filename (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p filename)
    (load-file filename)))


;;; Function, variable, and macro definitions

(require 'bb-defs)
(require 'bb-macros)


;;; Packages that should be enabled early

(use-package general
  :config
  (setq general-override-states '(normal visual motion))
  (general-override-mode))

(use-package no-littering)

(use-package popwin
  :config
  (setq popwin:special-display-config nil)
  (popwin-mode))


;;; Theme

(setq monokai-height-minus-1 1.0
      monokai-height-plus-1 1.0
      monokai-height-plus-2 1.0
      monokai-height-plus-3 1.0
      monokai-height-plus-4 1.0)

(load-theme 'monokai 'noconfirm)
(require 'color)

(bb-after-display
  (set-face-attribute 'default nil :font bb-font :height 100)
  (set-face-attribute 'region nil :background (color-lighten-name monokai-gray 10)))
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


;;; Modeline

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
     python-env
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


;;; General Emacs settings (built-ins, etc.)

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

(blink-cursor-mode -1)

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

(use-package outline
  :defer t
  :diminish outline-minor-mode)

(use-package recentf
  :init
  (setq recentf-max-saved-items 10000)
  :config
  (recentf-mode))

(use-package simple
  :defer t
  :diminish auto-fill-function)

(use-package smerge-mode
  :defer t
  :diminish (smerge-mode . "[sm]"))

(use-package tree-widget
  :defer t
  :config
  (bb-after-display
    (setq tree-widget-image-enable (display-images-p))))

(use-package uniquify)

(use-package winner
  :config
  (winner-mode))

(use-package whitespace
  :defer t
  :diminish (whitespace-mode . "w")
  :init
  (bb-leader ("tw" 'whitespace-mode "Toggle whitespace mode")))

(bb-leader
  ("<tab>" 'bb-alternate-buffer "Switch to previous buffer")
  (";" 'eval-expression "Evaluate elisp expression in minibuffer")
  ("bd" 'bb-kill-buffer "Kill buffer")
  ("fd" 'bb-kill-buffer-file "Kill buffer and delete file")
  ("fs" 'save-buffer "Save buffer")
  ("fy" 'bb-show-and-copy-filename "Show and copy filename to clipboard")
  ("hdc" 'describe-char "Describe character")
  ("hdf" 'describe-function "Describe function")
  ("hdF" 'describe-face "Describe face")
  ("hdl" 'bb-display-leaders "Display leader bindings")
  ("hdk" 'describe-key "Describe key")
  ("hdv" 'describe-variable "Describe variable")
  ("hi" 'bb-find-init "Go to init.el")
  ("hs" 'bb-find-scratch "Go to scratch buffer")
  ("td" 'bb-toggle-debug-on-error "Toggle debug-on-error")
  ("u" 'universal-argument "Universal argument")
  ("w" 'hydra-windows/body "Window management hydra"))


;;; Description of major-mode leader bindings

(bb-assign-leader "==" nil "Align defun/paragraph")
(bb-assign-leader "cc" nil "Build project")
(bb-assign-leader "cv" nil "Show build progress")
(bb-assign-leader "cb" nil "Evaluate buffer")
(bb-assign-leader "cf" nil "Evaluate defun")
(bb-assign-leader "cm" nil "Evaluate macro")
(bb-assign-leader "cs" nil "Evaluate expression")
(bb-assign-leader "l"  nil "Structured editing hydra")
(bb-assign-leader "qq" nil "Quit background connections")
(bb-assign-leader "te" nil "Toggle engine")
(bb-assign-leader "va" nil "Activate virtual environment")
(bb-assign-leader "vd" nil "Dectivate virtual environment")


;;; Evil and Co.

(use-package evil
  :init
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box)
        evil-insert-state-cursor '("chartreuse3" (bar . 2))
        evil-emacs-state-cursor '("SkyBlue2" box)
        evil-replace-state-cursor '("chocolate" (hbar . 2))
        evil-visual-state-cursor '("gray" (hbar . 2))
        evil-motion-state-cursor '("plum3" box)
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t)
  :config
  (evil-mode)
  (bb-add-hook evil-insert-state-exit-hook
    (deactivate-mark))

  ;; Miscellaneous keybindings
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
  (define-key evil-normal-state-map (kbd "] s") 'bb-insert-spaces-after)

  ;; Other
  (evil-set-command-property 'xref-find-definitions :jump t)
  (define-key evil-visual-state-map (kbd ">") 'bb-shift-right)
  (define-key evil-visual-state-map (kbd "<") 'bb-shift-left))

(use-package evil-args
  :defer t
  :init
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list
        '(elisp-mode))
  (evil-collection-init))

(use-package evil-collection-elisp-mode
  :defer t
  :config
  (advice-add 'eval-last-sexp :around 'evil-collection-elisp-mode-last-sexp))

(use-package evil-embrace
  :after evil-surround
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-escape
  :diminish evil-escape-mode
  :config
  (push 'emacs evil-escape-excluded-states)
  (unless keyboardiop
    (evil-escape-mode))
  (define-key minibuffer-local-map [escape] 'evil-escape)
  (define-key minibuffer-local-ns-map [escape] 'evil-escape)
  (define-key minibuffer-local-completion-map [escape] 'evil-escape)
  (define-key minibuffer-local-must-match-map [escape] 'evil-escape)
  (define-key minibuffer-local-isearch-map [escape] 'evil-escape))

(use-package evil-indent-plus
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-little-word
  :after evil)

(use-package evil-nerd-commenter
  :defer t
  :init
  (bb-leader
    ("cl" 'evilnc-comment-or-uncomment-lines "Comment lines")
    ("cp" 'evilnc-comment-or-uncomment-paragraphs "Comment paragraphs")
    ("cy" 'evilnc-copy-and-comment-lines "Copy and comment")))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode))

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


;;; Company and Co.

(use-package company
  :defer t
  :diminish (company-mode . "c")
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-require-match nil
        company-tooltip-align-annotations t
        company-tooltip-minimum-width 60)
  (bb-leader ("tc" 'company-mode "Toggle auto-completion"))
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


;;; Flycheck and Co.

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :diminish (flycheck-mode . "f")
  :init
  (setq-default flycheck-check-syntax-automatically nil)
  (bb-leader
    ("tf" 'flycheck-mode "Toggle flycheck")
    ("el" 'flycheck-list-errors "List flycheck errors")
    ("eb" 'flycheck-buffer "Run flycheck on buffer")
    ("ec" 'flycheck-clear "Clear flycheck errors"))
  (bb-popwin flycheck-error-list-mode)
  :config
  (aset flycheck-error-list-format 5 '("Message" 0 t)))


;;; Helm and Co.

(use-package helm
  :diminish helm-mode
  :init
  (setq helm-display-function 'bb-helm-display-child-frame
        helm-display-buffer-reuse-frame t
        helm-display-buffer-width 120
        helm-display-buffer-height 25)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  (bb-leader
    ("SPC" 'helm-M-x "Run command")
    ("bb" 'helm-mini "Switch to buffer")
    ("ff" 'helm-find-files "Find file")
    ("fl" 'helm-locate-library "Find elisp library")
    ("ji" 'helm-imenu "Find location in file")
    ("rl" 'helm-resume "Show last helm session"))
  (push "\\*helm.+\\*" bb-useless-buffers-regexp)
  :config
  (require 'bb-helm)
  (helm-mode)
  (helm-autoresize-mode)
  (define-key helm-map bb-right 'helm-maybe-exit-minibuffer)
  (define-key helm-map bb-down 'helm-next-line)
  (define-key helm-map bb-up 'helm-previous-line)
  (set-face-attribute 'helm-prefarg nil :foreground "PaleGreen"))

(use-package helm-ag
  :defer t
  :init
  (bb-leader ("/" 'bb-helm-ag-project "Search in project"))
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
    ("pb" 'helm-projectile-switch-to-buffer "Switch to project buffer")
    ("pd" 'helm-projectile-find-dir "Find project directory")
    ("pf" 'helm-projectile-find-file "Find project file")
    ("ph" 'helm-projectile "Projectile helm session")
    ("pp" 'helm-projectile-switch-project "Find project"))
  :config
  (define-key helm-projectile-find-file-map bb-right 'helm-maybe-exit-minibuffer))

(use-package helm-swoop
  :init
  (setq helm-swoop-split-with-multiple-windows t
        helm-swoop-pre-input-function (lambda () ""))
  (bb-leader ("ss" 'bb-helm-swoop "Search in file")))

(use-package helm-xref
  :after xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))


;;; IRC and Co.

(use-package bb-erc
  :commands (bb-erc)
  :init
  (setq erc-timestamp-format-let "\n%A %B %e, %Y\n\n"
        erc-timestamp-format-right "%H:%M"
        erc-timestamp-right-column 80
        erc-prompt-for-nickserv-password nil
        erc-image-inline-rescale 200
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
        erc-track-position-in-mode-line t
        erc-track-exclude-server-buffer t
        erc-track-exclude-types
        '("JOIN" "NICK" "PART" "QUIT" "MODE" "305" "324" "329" "332" "333" "353" "477")
        erc-track-shorten-function
        (lambda (names) (and names (list (propertize "!" 'face 'erc-notice-face))))
        erc-track-use-faces nil
        erc-join-buffer 'bury)
  (bb-leader
    ("ai" 'bb-erc "Open IRC")
    ("bi" 'erc-track-switch-buffer "Switch to IRC buffer with unread messages"))
  (bb-mm-leader erc-mode "qq" 'erc-quit-server)
  (evil-set-initial-state 'erc-mode 'normal)
  (bb-add-hook erc-mode-hook
    (setq-local global-hl-line-mode nil)))


;;; LSP and Co.

(use-package lsp-mode
  :commands (lsp-make-traverser)
  :diminish (lsp-mode . "l")
  :defer t
  :init
  (setq lsp-highlight-symbol-at-point nil)
  (bb-leader ("tl" 'lsp-mode "Toggle LSP"))
  (define-key evil-insert-state-map (kbd "C-l") 'company-complete)
  :config
  (set-face-attribute 'lsp-face-highlight-textual nil
    :background monokai-highlight-line))

(use-package lsp
  :defer t
  :init
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-flycheck-live-reporting nil
        lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil)
  (evil-define-key 'motion lsp-ui-mode-map "gr" 'lsp-ui-peek-find-references)
  (bb-leader ("tr" 'lsp-ui-sideline-mode "Toggle LSP sideline")))

(use-package lsp-ui-peek
  :defer t
  :init
  (add-hook 'lsp-ui-peek-mode-hook 'evil-normalize-keymaps)
  :config
  (evil-make-intercept-map lsp-ui-peek-mode-map)
  (define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map bb-down 'lsp-ui-peek--select-next-file)
  (define-key lsp-ui-peek-mode-map bb-up 'lsp-ui-peek--select-prev-file)
  (define-key lsp-ui-peek-mode-map bb-right 'lsp-ui-peek--goto-xref))


;;; Magit and Co.

(use-package magit
  :defer t
  :init
  (setq magit-bury-buffer-function 'magit-mode-quit-window)
  (bb-leader ("gs" 'magit-status "Open Magit status"))
  (push "magit.*" bb-useless-buffers-regexp))

(use-package evil-magit
  :after magit)

(use-package forge
  :after magit
  :config
  (evil-define-key '(normal visual) magit-mode-map "," 'forge-dispatch)
  (evil-define-key 'normal magit-commit-section-map (kbd "gb") 'forge-browse-dwim)
  (evil-define-key 'normal magit-remote-section-map (kbd "gb") 'forge-browse-remote)
  (evil-define-key 'normal magit-branch-section-map (kbd "gb") 'forge-browse-branch)
  (evil-define-key 'normal forge-topic-mode-map (kbd "C-c C-c") 'forge-create-post))

(use-package with-editor
  :defer t
  :diminish with-editor-mode)


;;; C/C++ and Co.

(use-package cc-mode
  :defer t
  :init
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp))

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

(use-package cquery
  :after lsp-clients)


;;; HTML and Co.

(use-package web-mode
  :mode ("\\.\\(dj\\)?html?\\'" "\\.xinp\\'")
  :init
  (bb-mm-leader web-mode "te" 'web-mode-set-engine)
  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 4)
  (add-hook 'web-mode-hook 'lsp))


;;; LaTeX and Co.

(use-package auctex-latexmk
  :after latex
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

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

(use-package tex-buf
  :defer t
  :diminish (compilation-in-progress . "[c]"))

(use-package tex-site
  ;; Not deferred, since tex-site.el is essentially an autoloads file.
  :init
  (setq tex-fontify-script nil
        font-latex-fontify-script nil
        TeX-parse-self nil)
  (add-hook 'latex-mode-hook 'TeX-PDF-mode)
  (bb-company LaTeX-mode
    company-reftex-labels company-reftex-citations
    company-auctex-macros company-auctex-environments)
  (bb-mm-leader latex-mode
    "cc" 'bb-latex-build
    "cv" 'bb-latex-check-compilation))


;;; Python and Co.

(use-package bb-py-all-env
  :commands (bb-py-all-env-activate bb-py-all-env-deactivate)
  :init
  (bb-mm-leader python-mode
    "va" 'bb-py-all-env-activate
    "vd" 'bb-py-all-env-deactivate))

(use-package conda
  :defer t
  :init
  (setq conda-anaconda-home (when (file-exists-p "~/miniconda3") (expand-file-name "~/miniconda3/")))
  (add-hook 'conda-postactivate-hook 'lsp-restart-workspace)
  (add-hook 'conda-postdeactivate-hook 'lsp-restart-workspace))

(use-package python
  :defer t
  :init
  (add-hook 'python-mode-hook 'lsp))

(use-package pyvenv
  :defer t
  :init
  (add-hook 'pyvenv-post-activate-hooks 'lsp-restart-workspace)
  (add-hook 'pyvenv-post-deactivate-hooks 'lsp-restart-workspace))


;;; Programming languages and other major modes

(use-package elisp-mode
  :defer t
  :init
  (bb-mm-leader emacs-lisp-mode
    "cs" 'eval-last-sexp
    "cf" 'eval-defun
    "cb" 'eval-buffer
    "l" 'hydra-structured-editing-lisp/body)
  (bb-company emacs-lisp-mode company-capf)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (bb-add-hook emacs-lisp-mode-hook
    (push '("Package" "\\(^\\s-*(use-package +\\)\\(\\_<[^ ]+\\_>\\)" 2) imenu-generic-expression)))

(use-package lisp-mode
  :defer t
  :init
  (bb-mm-leader lisp-mode "l" 'hydra-structured-editing-lisp/body))

(use-package powershell-mode
  :mode "\\.ps1\\'")

(use-package text-mode
  :hook (text-mode . bb-maybe-auto-fill-mode))


;;; Miscellaneous

(use-package ace-window
  :defer t
  :init
  (setq aw-keys '(?s ?d ?f ?g ?h ?j ?k ?l))
  (bb-leader ("gw" 'ace-window "goto window")))

(use-package avy
  :defer t
  :init
  (bb-leader ("gl" 'avy-goto-line "goto line")))

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package expand-region
  :defer t
  :init
  (bb-leader ("vv" 'er/expand-region "Run expand-region")))

(use-package eyebrowse
  :init
  (setq eyebrowse-new-workspace t
        eyebrowse-wrap-around t)
  :config
  (define-key evil-motion-state-map (kbd "<C-next>") 'eyebrowse-next-window-config)
  (define-key evil-motion-state-map (kbd "<C-prior>") 'eyebrowse-prev-window-config)
  (eyebrowse-mode))

(use-package hierarchy
  :defer t
  :config
  (evil-set-initial-state 'hierarchy-tabulated-mode 'motion))

(use-package highlight-operators
  :hook (prog-mode . highlight-operators-mode)
  :init
  (bb-advise-except-derived-modes highlight-operators-mode
    lisp-mode scheme-mode emacs-lisp-mode python-mode)
  :config
  (set-face-attribute 'highlight-operators-face nil
    :inherit 'font-lock-keyword-face))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package lispy
  :diminish (lispy-mode . "L")
  :hook ((lisp-mode scheme-mode emacs-lisp-mode) . lispy-mode)
  :config
  (lispy-define-key lispy-mode-map "o" 'lispy-different)
  (lispy-define-key lispy-mode-map ">" 'lispy-slurp-or-barf-right)
  (lispy-define-key lispy-mode-map "<" 'lispy-slurp-or-barf-left))

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
  (setq page-break-lines-modes '(prog-mode julia-mode-prog-mode))
  (global-page-break-lines-mode))

(use-package projectile
  :diminish projectile-mode
  :init
  (bb-leader ("ga" 'projectile-find-other-file "Find alternate project file"))
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (push '("C" . ("h")) projectile-other-file-alist))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook ((prog-mode LaTeX-mode) . smartparens-mode)
  :diminish (smartparens-mode . "s")
  :init
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (bb-leader ("ts" 'smartparens-mode "Toggle smartparens"))
  :config
  (bb-advise-except-derived-modes smartparens-mode
    lisp-mode scheme-mode emacs-lisp-mode web-mode)
  (sp-local-pair '(c-mode c++-mode) "'" nil :post-handlers '(:rem sp-escape-quotes-after-insert))
  (bb-apply-newline-indent (c-mode c++-mode python-mode) "{" "[" "(")
  (bb-apply-newline-indent (css-mode scss-mode) "{")
  (bb-apply-newline-indent (js-mode) "{"))

(use-package smartparens-config
  :after smartparens)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (setq undo-tree-enable-undo-in-region nil)
  (bb-leader ("au" 'undo-tree-visualize "Show undo history"))
  :config
  (global-undo-tree-mode))

(use-package vterm
  :commands (vterm)
  :init
  (evil-set-initial-state 'vterm-mode 'emacs)
  (bb-add-hook vterm-mode-hook
    (setq-local global-hl-line-mode nil)
    (setq-local truncate-lines t))
  (setq vterm-keymap-exceptions '("<f12>"))
  (global-set-key (kbd "<f12>") 'bb-vterm)
  :config
  (set-face-attribute 'vterm-color-black nil :background monokai-comments :foreground monokai-comments)
  (set-face-attribute 'vterm-color-blue nil :background monokai-blue :foreground monokai-blue)
  (set-face-attribute 'vterm-color-green nil :background monokai-green :foreground monokai-green)
  (set-face-attribute 'vterm-color-red nil :background monokai-red :foreground monokai-red)
  (set-face-attribute 'vterm-color-yellow nil :background monokai-yellow :foreground monokai-yellow)
  (set-face-attribute 'vterm-color-magenta nil :background monokai-magenta :foreground monokai-magenta)
  (set-face-attribute 'vterm-color-cyan nil :background monokai-cyan :foreground monokai-cyan))

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

(use-package yeast
  :defer t
  :hook ((c-mode c++-mode python-mode) . yeast-mode)
  :config
  (evil-define-key 'normal yeast-mode-map (kbd "M-n") 'yeast-select-at-point)
  (evil-define-key 'visual yeast-mode-map (kbd "M-j") 'yeast-select-next-at-point)
  (evil-define-key 'visual yeast-mode-map (kbd "M-k") 'yeast-select-prev-at-point)
  (evil-define-key 'visual yeast-mode-map (kbd "M-h") 'yeast-select-parent-at-point)
  (evil-define-key 'visual yeast-mode-map (kbd "M-l") 'yeast-select-first-child-at-point)
  (add-hook 'yeast-mode-hook 'evil-normalize-keymaps))


;;; Load customizations, if any

(load custom-file)


;;; init.el ends here
