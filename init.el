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
      custom-theme-directory user-emacs-directory

      load-prefer-newer t

      evil-want-keybinding nil

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
      bb-font "Iosevka Custom Bold")

(push (concat user-emacs-directory "lisp") load-path)
(push (concat user-emacs-directory "lib/borg") load-path)
(require 'borg)
(borg-initialize)


(eval-when-compile
  (require 'lsp-clients))


;;; Local settings, if any

(let ((filename (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p filename)
    (load-file filename)))


;;; Packages that should be enabled early

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package general
  :config
  (setq general-override-states '(normal visual motion))
  (general-override-mode))

(use-package no-littering)

(use-package popwin
  :config
  (setq popwin:special-display-config nil)
  (popwin-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))


;;; Function, variable, and macro definitions

(require 'bb-defs)
(require 'bb-macros)


;;; Theme

(setq monokai-height-minus-1 1.0
      monokai-height-plus-1 1.0
      monokai-height-plus-2 1.0
      monokai-height-plus-3 1.0
      monokai-height-plus-4 1.0
      monokai-red "#ff4185"
      monokai-doc-face-as-comment t)
(load-theme 'monokai 'noconfirm)

(bb-after-display
  (set-face-attribute 'default nil :font bb-font :height 100))
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-string-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil :foreground "#ffabd6" :weight 'bold)
(set-face-attribute 'mode-line nil :background monokai-background)
(set-face-attribute 'mode-line-emphasis nil :foreground "#ffabd6" :weight 'ultra-bold)
(set-face-attribute 'region nil :background monokai-gray)

(with-eval-after-load 'doom-modeline
  (set-face-attribute 'doom-modeline-evil-emacs-state nil :foreground "SkyBlue2")
  (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "chartreuse3")
  (set-face-attribute 'doom-modeline-evil-normal-state nil :foreground "DarkGoldenrod2")
  (set-face-attribute 'doom-modeline-evil-operator-state nil :foreground "DarkGoldenrod2")
  (set-face-attribute 'doom-modeline-evil-replace-state nil :foreground "chocolate")
  (set-face-attribute 'doom-modeline-evil-visual-state nil :foreground "gray")
  (set-face-attribute 'doom-modeline-evil-motion-state nil :foreground "plum3")
  (set-face-attribute 'doom-modeline-project-dir nil :italic nil))

(with-eval-after-load 'company
  (set-face-attribute 'company-tooltip-selection nil :extend t))
(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil :extend t))
(with-eval-after-load 'magit
  (set-face-attribute 'magit-diff-hunk-heading nil :extend t)
  (set-face-attribute 'magit-diff-hunk-heading-highlight nil :extend t)
  (set-face-attribute 'magit-diff-added-highlight nil :extend t)
  (set-face-attribute 'magit-diff-context-highlight nil :extend t)
  (set-face-attribute 'magit-diff-removed-highlight nil :extend t)
  (set-face-attribute 'magit-section-highlight nil :extend t))
(with-eval-after-load 'org
  (set-face-attribute 'org-block nil :extend t)
  (set-face-attribute 'org-block-begin-line nil :extend t)
  (set-face-attribute 'org-block-end-line nil :extend t))

;; (use-package dimmer
;;   :init
;;   (setq dimmer-fraction 0.25)
;;   :config
;;   (push 'bb-dimmer-predicate dimmer-prevent-dimming-predicates)
;;   (dimmer-configure-which-key)
;;   (dimmer-mode))


;;; Modeline

(use-package doom-modeline
  :init
  (bb-after-display
    (setq doom-modeline-icon t))
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-column-zero-based nil
        doom-modeline-buffer-encoding nil
        doom-modeline-minor-modes t
        doom-modeline-project-detection 'projectile)
  (setq-default doom-modeline-env-python-parser-fn 'bb-py-all-env-parse)
  :config
  (doom-modeline-mode))


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

(setq-default fill-column 70
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

(use-package compile
  :defer t
  :init
  (bb-popwin compilation-mode)
  (setq compilation-scroll-output 'first-error)
  :config
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook 'bb-compilation-filter))

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


;;; Leader bindings, categories and descriptions

(bb-leader
  ("<tab>" 'bb-alternate-buffer "Prev buf")
  (";" 'eval-expression "Eval elisp")
  ("bd" 'bb-kill-buffer "Kill buf")
  ("fd" 'bb-kill-buffer-file "Kill buf, del file")
  ("fs" 'save-buffer "Save buf")
  ("fy" 'bb-show-and-copy-filename "Copy filename")
  ("hdc" 'describe-char "Desc character")
  ("hdf" 'describe-function "Desc function")
  ("hdF" 'describe-face "Desc face")
  ("hdk" 'describe-key "Desc key")
  ("hdv" 'describe-variable "Desc var")
  ("hi" 'bb-find-init "init.el")
  ("hs" 'bb-find-scratch "Scratch buf")
  ("td" 'bb-toggle-debug-on-error "Toggle debug-on-error")
  ("u" 'universal-argument "Universal arg")
  ("w" 'hydra-windows/body "Window mgmt"))

(which-key-add-key-based-replacements
  "SPC a" "Apps"
  "SPC b" "Buffers"
  "SPC c" "Compile"
  "SPC e" "Errors"
  "SPC f" "Files"
  "SPC h" "Help"
  "SPC hd" "Describe"
  "SPC j" "Jump"
  "SPC p" "Project"
  "SPC q" "Quit"
  "SPC s" "Search"
  "SPC t" "Toggle"
  "SPC v" "Venvs")

(which-key-add-key-based-replacements
  "SPC ==" "Align"
  "SPC cc" "Build"
  "SPC cv" "Show build"
  "SPC cb" "Eval buf"
  "SPC cf" "Eval fun"
  "SPC cm" "Eval macro"
  "SPC cs" "Eval expr"
  "SPC l" "Struct-ed"
  "SPC qq" "Quit connections"
  "SPC te" "Engine"
  "SPC va" "Activate"
  "SPC vd" "Deactivate")


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
        evil-want-C-u-scroll t)
  :config

  (unless (fboundp 'evil-bind-key)
    (defun evil-bind-key (state keymap &rest body)
     (if (and (symbolp keymap) (not (memq keymap '(local global))))
         (apply #'evil-define-minor-mode-key state keymap body)
       (apply #'evil-define-key* state keymap body))))

  ;; Use undo-fu as undo provider if we have the right evil branch
  (when (boundp 'evil-undo-provider)
    (setq evil-undo-provider 'undo-fu))

  (evil-mode)
  (bb-add-hook evil-insert-state-exit-hook
    (deactivate-mark))

  ;; Miscellaneous keybindings
  (define-key evil-motion-state-map (kbd "<left>") 'windmove-left)
  (define-key evil-motion-state-map (kbd "<down>") 'windmove-down)
  (define-key evil-motion-state-map (kbd "<up>") 'windmove-up)
  (define-key evil-motion-state-map (kbd "<right>") 'windmove-right)

  (define-key evil-motion-state-map (kbd "gd") 'xref-find-definitions)
  (define-key evil-motion-state-map (kbd "gD") 'xref-find-definitions-other-window)
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
  (define-key evil-visual-state-map (kbd "<") 'bb-shift-left)

  (add-hook 'after-change-major-mode-hook 'bb-set-evil-shift-width)
  (add-hook 'hack-local-variables-hook 'bb-set-evil-shift-width))

(use-package evil-args
  :defer t
  :init
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list
        '(compile
          ediff
          elisp-mode
          flycheck
          magit
          magit-todos))
  (evil-collection-init))

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
  (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)
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
    (evil-bind-key 'visual evil-smartparens-mode-map (kbd "o") nil))
  (bb-advise before evil-delete-backward-char-and-join (count &rest _args)
    (save-match-data
      (sp-delete-pair count))))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode)
  :config
  (evil-bind-key 'visual evil-surround-mode-map "s" 'evil-surround-region))


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

(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (require 'all-the-icons)
  (setf (alist-get 'min-height company-box-frame-parameters) 6)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-backends-colors nil

        ;; These are the Doom Emacs defaults
        company-box-icons-all-the-icons
        `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
          (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
          (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
          (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
          (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
          (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
          (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
          (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
          (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
          (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
          (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
          (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
          (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
          (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
          (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
          (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
          (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
          (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
          (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
          (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
          (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
          (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
          (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))))

  ;; Add a space after the icon
  (dolist (elt company-box-icons-all-the-icons)
    (setcdr elt (concat (cdr elt) " "))))


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


;;; Ivy and Co.

(use-package ivy
  :diminish ivy-mode
  :init
  (bb-leader ("rl" 'ivy-resume "Resume ivy"))
  (setq ivy-format-functions-alist
        '((t . ivy-format-function-line)))
  :config
  (ivy-mode)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-scroll-down-command)
  (define-key ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-up-command)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "M-l") 'ivy-dispatching-done)
  (define-key ivy-minibuffer-map (kbd "C-M-l") 'ivy-immediate-done))

(use-package ivy-posframe
  :diminish ivy-posframe-mode
  :after ivy
  :init
  (setq ivy-posframe-display-functions-alist
        '((t . bb-ivy-posframe-display-frame-top-center)))
  :config
  (ivy-posframe-mode))

(use-package counsel
  :defer t
  :init
  (bb-leader
    ("SPC" 'counsel-M-x "Run command")
    ("/" 'counsel-ag "Search in project")
    ("bb" 'counsel-ibuffer "Switch buffer")
    ("ff" 'counsel-find-file "Find file")
    ("fl" 'counsel-find-library "Find library")
    ("fr" 'counsel-recentf "Recent files")
    ("ji" 'counsel-imenu "Find location in file"))
  :config
  (define-key counsel-ag-map (kbd "M-l") 'ivy-call-and-recenter)
  (define-key counsel-ag-map (kbd "C-l") 'ivy-done)
  (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
  (define-key counsel-find-file-map (kbd "C-l") 'ivy-alt-done)
  (define-key counsel-imenu-map (kbd "M-l") 'ivy-call-and-recenter)
  (define-key counsel-imenu-map (kbd "C-l") 'ivy-done))

(use-package counsel-projectile
  :defer t
  :init
  (bb-leader
    ("pb" 'counsel-projectile-switch-to-buffer "Switch to project buffer")
    ("pd" 'counsel-projectile-find-dir "Find project directory")
    ("pf" 'counsel-projectile-find-file "Find project file")
    ("ph" 'counsel-projectile "Projectile")
    ("pp" 'counsel-projectile-switch-project "Find project")))

(use-package swiper
  :defer t
  :init
  (bb-leader ("ss" 'swiper "Search in buffer"))
  :config
  (define-key swiper-map (kbd "C-l") 'ivy-done))


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

(use-package lsp
  :diminish lsp-mode
  :defer t
  :init
  (setq lsp-auto-guess-root t
        lsp-diagnostic-package :flycheck
        lsp-enable-snippet nil
        lsp-flycheck-live-reporting nil
        lsp-log-io nil
        lsp-signature-auto-activate nil)
  (bb-leader ("tl" 'lsp-mode "Toggle LSP")
             ("vr" 'lsp-restart-workspace "Restart LSP"))
  (define-key evil-insert-state-map (kbd "C-l") 'company-complete)

  :config
  (remove-hook 'lsp-eldoc-hook 'lsp-document-highlight)
  (set-face-attribute 'lsp-face-highlight-textual nil
    :background monokai-highlight-line)
  (when (executable-find "clangd-9")
    (setq lsp-clients-clangd-executable "clangd-9")))

(use-package lsp-dart
  :defer t
  :config
  (bb-lsp-set-priority 'dart_analysis_server -10))

(use-package lsp-clients
  :defer t
  :config
  (bb-lsp-set-priority 'clangd 1))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-enable-symbol-highlighting nil)
  (bb-leader ("tr" 'lsp-ui-sideline-mode "Toggle LSP sideline"))
  :config
  (evil-bind-key 'motion lsp-ui-mode-map "gr" 'lsp-ui-peek-find-references))

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
  (bb-leader
    ("gf" 'magit-file-dispatch "Git file actions")
    ("gs" 'magit-status "Open Magit status")
    ("gb" 'magit-blame "Git blame buffer"))
  (push "magit.*" bb-useless-buffers-regexp))

(use-package evil-magit
  :after magit)

(use-package forge
  :after magit
  :config
  (evil-bind-key '(normal visual) magit-mode-map "," 'forge-dispatch)
  (evil-bind-key 'normal magit-commit-section-map (kbd "gb") 'forge-browse-dwim)
  (evil-bind-key 'normal magit-remote-section-map (kbd "gb") 'forge-browse-remote)
  (evil-bind-key 'normal magit-branch-section-map (kbd "gb") 'forge-browse-branch)
  (evil-bind-key 'normal forge-topic-mode-map (kbd "C-c C-c") 'forge-create-post))

(use-package forge-list
  :defer t
  :config
  (evil-bind-key 'normal forge-topic-list-mode-map (kbd "q") 'quit-window)
  (evil-bind-key 'normal forge-topic-list-mode-map (kbd "o") 'forge-browse-topic))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode))

(use-package with-editor
  :defer t
  :diminish with-editor-mode)


;;; Org and Co.

(use-package org
  :defer t
  :init
  (setq org-src-window-setup 'current-window
        org-adapt-indentation nil)
  (add-hook 'org-src-mode-hook 'evil-normalize-keymaps 'end)
  :config
  ;; Use C-c C-c as a 'commit' binding when editing org source blocks
  (evil-bind-key 'normal org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit))

(use-package evil-org
  :hook (org-mode . evil-org-mode))


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

(use-package ccls
  :if (bb-has-executable-p 'lsp-cc-ccls)
  :after lsp)

(use-package cquery
  ;; Use cquery if ccls is not available
  :if (and (not (bb-has-executable-p 'lsp-cc-ccls))
           (bb-has-executable-p 'lsp-cc-cquery))
  :after lsp)


;;; HTML and Co.

(use-package web-mode
  :mode ("\\.\\(dj\\)?html?\\'" "\\.xinp\\'")
  :init
  (bb-mm-leader web-mode "te" 'web-mode-set-engine)
  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 4)
  (when (bb-has-executable-p 'lsp-html)
    (add-hook 'web-mode-hook 'lsp))
  :config
  (set-face-attribute 'web-mode-comment-face nil :slant 'italic))


;;; Julia and Co.

(use-package julia-mode
  :defer t
  :init
  (add-hook 'julia-mode-hook 'lsp))

(use-package lsp-julia
  :after lsp
  :init
  (setq lsp-julia-package-dir (no-littering-expand-var-file-name "lsp-julia/")
        lsp-julia-default-environment "~/.julia/environments/v1.3"))


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

(use-package tex-mode
  :defer t
  :config
  (with-eval-after-load 'smartparens
    (bb-apply-newline-indent (latex-mode) "{" ("\\[" "\\]"))))

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
    "cv" 'bb-latex-check-compilation
    "ie" 'LaTeX-environment))


;;; Python and Co.

(use-package bb-py-all-env
  :commands (bb-py-all-env-activate bb-py-all-env-deactivate bb-py-all-env-parse)
  :init
  (bb-mm-leader python-mode
    "va" 'bb-py-all-env-activate
    "vd" 'bb-py-all-env-deactivate))

(use-package conda
  :defer t
  :init
  (setq conda-anaconda-home
        (if (file-exists-p "~/source/miniconda3")
            (expand-file-name "~/source/miniconda3/")
          (when (file-exists-p "~/miniconda3")
            (expand-file-name "~/miniconda3"))))
  (add-hook 'conda-postactivate-hook 'lsp-restart-workspace)
  (add-hook 'conda-postdeactivate-hook 'lsp-restart-workspace)
  (add-hook 'conda-postactivate-hook 'doom-modeline-env-update-python)
  (add-hook 'conda-postdeactivate-hook 'doom-modeline-env-update-python))

;; (use-package lsp-python-ms
;;   :after python)

(use-package python
  :defer t
  :init
  (add-hook 'python-mode-hook 'lsp)
  (setq-default python-shell-interpreter "python3"))

(use-package pyvenv
  :defer t
  :init
  (add-hook 'pyvenv-post-activate-hooks 'lsp-restart-workspace)
  (add-hook 'pyvenv-post-deactivate-hooks 'lsp-restart-workspace)
  (add-hook 'conda-postactivate-hook 'doom-modeline-env-update-python)
  (add-hook 'conda-postdeactivate-hook 'doom-modeline-env-update-python))


;;; Programming languages and other major modes

(use-package dart-mode
  :defer t
  :init
  (add-hook 'dart-mode-hook 'lsp))

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

(use-package lua-mode
  :defer t
  :init
  (setq lua-indent-level 2))

(use-package po-mode
  :defer t
  :config
  (evil-set-initial-state 'po-mode 'motion)
  (evil-bind-key 'motion po-mode-map "j" 'po-next-entry)
  (evil-bind-key 'motion po-mode-map "k" 'po-previous-entry)
  (evil-bind-key 'motion po-mode-map "e" 'po-edit-msgstr)
  (evil-bind-key 'motion po-mode-map "u" 'po-undo)
  (evil-bind-key 'motion po-mode-map "u" 'po-undo))

(use-package powershell-mode
  :mode "\\.ps1\\'")

(use-package text-mode
  :hook (text-mode . bb-maybe-auto-fill-mode))


;;; Miscellaneous

(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package ace-window
  :defer t
  :init
  (setq aw-keys '(?s ?d ?f ?g ?h ?j ?k ?l))
  (bb-leader ("jw" 'ace-window "goto window")))

(use-package avy
  :defer t
  :init
  (bb-leader ("jl" 'avy-goto-line "goto line")))

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
  (define-key evil-emacs-state-map (kbd "<C-next>") 'eyebrowse-next-window-config)
  (define-key evil-emacs-state-map (kbd "<C-prior>") 'eyebrowse-prev-window-config)
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
  (lispy-define-key lispy-mode-map "<" 'lispy-slurp-or-barf-left)
  (define-key lispy-mode-map (kbd "C-j") 'lispy-split)
  (define-key lispy-mode-map (kbd "M-j") 'lispy-newline-and-indent))

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
  (bb-leader
    ("ga" 'projectile-find-other-file "Find alternate project file")
    ("pC" 'projectile-configure-project "Configure")
    ("pc" 'projectile-compile-project "Compile")
    ("pr" 'projectile-run-project "Run")
    ("pt" 'projectile-test-project "Test"))
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
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
  (bb-apply-newline-indent (c-mode c++-mode lua-mode python-mode) "{" "[" "(")
  (bb-apply-newline-indent (css-mode scss-mode) "{")
  (bb-apply-newline-indent (js-mode typescript-mode) "{")
  ;; Workaround smartparens #963
  ;; (push 'c-electric-brace sp--special-self-insert-commands)
  ;; (push 'c-electric-paren sp--special-self-insert-commands)
  )

(use-package smartparens-config
  :after smartparens)

(use-package typo
  :hook (org-mode . typo-mode))

(use-package tree-sitter
  :diminish (tree-sitter-mode . "T")
  :hook ((c-mode
          c++-mode
          css-mode
          js-mode
          json-mode
          julia-mode
          python-mode
          rust-mode
          typescript-mode)
         . tree-sitter-mode)
  :init
  (bb-leader ("tT" 'tree-sitter-mode "Toggle tree-sitter"))
  :config
  (require 'tree-sitter-langs))

(use-package undo-tree
  :if (or (not (boundp 'evil-undo-provider))
          (eq 'undo-tree (bound-and-true-p evil-undo-provider)))
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
  ;; Default value plus F12
  (setq vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y" "<f12>"))
  (global-set-key (kbd "<f12>") 'bb-vterm)
  :config
  (set-face-attribute 'vterm-color-black nil :background monokai-comments :foreground monokai-comments)
  (set-face-attribute 'vterm-color-blue nil :background monokai-blue :foreground monokai-blue)
  (set-face-attribute 'vterm-color-green nil :background monokai-green :foreground monokai-green)
  (set-face-attribute 'vterm-color-red nil :background monokai-red :foreground monokai-red)
  (set-face-attribute 'vterm-color-yellow nil :background monokai-yellow :foreground monokai-yellow)
  (set-face-attribute 'vterm-color-magenta nil :background monokai-magenta :foreground monokai-magenta)
  (set-face-attribute 'vterm-color-cyan nil :background monokai-cyan :foreground monokai-cyan))

(use-package which-key-posframe
  :init
  (setq which-key-posframe-poshandler 'posframe-poshandler-point-bottom-left-corner)
  :config
  (which-key-posframe-mode))

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
  (bb-leader ("ty" 'yas-minor-mode "Toggle yasnippet"))
  :config
  (yas-reload-all))


;;; Load customizations, if any

(load custom-file)


;;; init.el ends here
