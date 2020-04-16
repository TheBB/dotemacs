;;; bb-defs.el --- Personal definitions. -*- lexical-binding: t -*-

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

;; Miscellaneous functions, macros and associated variables needed in
;; my Emacs init.

;;; Code:


(eval-when-compile
  (require 'cl-lib)
  (require 'hydra)
  (require 'ivy)
  (require 'lsp)
  (require 'bb-macros))

(require 'ht)


(declare-function ansi-color-apply-on-region "ansi-color")
(declare-function bufler-group-tree-leaf-path "ext:bufler-group-tree")
(declare-function bufler-buffers "ext:bufler")
(declare-function bufler-buffer-alist-at "ext:bufler")
(declare-function bufler-workspace-buffers "ext:bufler-workspace")
(declare-function counsel-find-file-action "ext:counsel")
(declare-function counsel-ibuffer--get-buffers "ext:counsel")
(declare-function counsel-ibuffer-visit-buffer "ext:counsel")
(declare-function counsel-projectile-action "ext:counsel-projectile")
(declare-function hydra-default-pre "ext:hydra")
(declare-function hydra-keyboard-quit "ext:hydra")
(declare-function hydra-set-transient-map "ext:hydra")
(declare-function hydra-show-hint "ext:hydra")
(declare-function hydra--call-interactively-remap-maybe "ext:hydra")
(declare-function evil-insert-newline-above "ext:evil-common")
(declare-function evil-insert-newline-below "ext:evil-common")
(declare-function evil-window-split "ext:evil-commands")
(declare-function evil-window-vsplit "ext:evil-commands")
(declare-function eyebrowse-create-window-config "ext:eyebrowse")
(declare-function eyebrowse-switch-to-window-config "ext:eyebrowse")
(declare-function eyebrowse-rename-window-config "ext:eyebrowse")
(declare-function eyebrowse-next-window-config "ext:eyebrowse")
(declare-function eyebrowse-prev-window-config "ext:eyebrowse")
(declare-function eyebrowse--get "ext:eyebrowse")
(declare-function ivy--get-window "ext:ivy")
(declare-function ivy-posframe--display "ext:ivy-posframe")
(declare-function macrostep-collapse "ext:macrostep")
(declare-function macrostep-next-macro "ext:macrostep")
(declare-function macrostep-prev-macro "ext:macrostep")
(declare-function macrostep-collapse-all "ext:macrostep")
(declare-function posframe-poshandler-frame-top-center "ext:posframe")
(declare-function projectile-project-root "ext:projectile")
(declare-function sp-wrap-with-pair "ext:smartparens")
(declare-function sp-forward-barf-sexp "ext:smartparens")
(declare-function sp-backward-barf-sexp "ext:smartparens")
(declare-function sp-forward-slurp-sexp "ext:smartparens")
(declare-function sp-backward-slurp-sexp "ext:smartparens")
(declare-function sp-forward-sexp "ext:smartparens")
(declare-function sp-backward-sexp "ext:smartparens")
(declare-function sp-forward-symbol "ext:smartparens")
(declare-function sp-backward-symbol "ext:smartparens")
(declare-function sp-kill-sexp "ext:smartparens")
(declare-function sp-kill-symbol "ext:smartparens")
(declare-function sp-kill-word "ext:smartparens")
(declare-function TeX-active-buffer "ext:tex-buf")
(declare-function TeX-command-master "ext:tex-buf")
(declare-function TeX-command "ext:tex-buf")
(declare-function TeX-recenter-output-buffer "ext:tex-buf")
(declare-function undo-tree-undo "ext:undo-tree")
(declare-function vterm "ext:vterm")
(declare-function winner-undo "winner")

(defvar counsel-ibuffer--buffer-name)
(defvar evil-shift-width)
(defvar eyebrowse-new-workspace)


;;; Executables

(defvar bb-executables
  '((lsp-cc-ccls
     (executable . "ccls")
     (version-cmd . "ccls --version")
     (version-regexp . "ccls version \\([0-9\\.]*\\)"))
    (lsp-cc-cquery
     (executable . "cquery"))
    (lsp-html
     (executable . "html-languageserver")
     (version-cmd . "npm list -g vscode-html-languageserver-bin")
     (version-regexp . "vscode-html-languageserver-bin@\\([0-9\\.]*\\)"))
    (lsp-julia
     (command . "julia -e 'using LanguageServer'")
     (version-cmd . "julia -e 'import Pkg; Pkg.status()'")
     (version-regexp . "LanguageServer v\\([0-9\\.+]*\\)"))
    (lsp-rust-rls
     (executable . "rls")
     (version-cmd . "rls --version")
     (version-regexp . "rls \\([0-9\\.]*\\)"))))

(defun bb-check-executable (exec)
  (let ((entry (assq exec bb-executables)))
    (unless (assq 'found (cdr entry))
      (let* ((executable (alist-get 'executable (cdr entry)))
             (command (alist-get 'command (cdr entry)))
             (version-cmd (alist-get 'version-cmd (cdr entry)))
             (version-regexp (alist-get 'version-regexp (cdr entry)))
             (found (or (and executable (executable-find executable))
                        (and command (= 0 (call-process-shell-command command)))))
             (version "?"))
        (when (and found version-cmd version-regexp)
          (let ((output (with-temp-buffer
                          (call-process-shell-command version-cmd nil t)
                          (buffer-string))))
            (when (string-match version-regexp output)
              (setq version (match-string 1 output)))))
        (push `(found . ,found) (cdr entry))
        (push `(version . ,version) (cdr entry))))
    (cdr entry)))

(defun bb-has-executable-p (exec)
  (alist-get 'found (bb-check-executable exec)))

(defun bb-executable-version (exec)
  (alist-get 'version (bb-check-executable exec)))


;;; Buffer predicate function

(defvar bb-useful-buffers-regexp nil
  "Regular expressions to determine if a buffer is useful.")

(defvar bb-useless-buffers-regexp
  '("\\*Messages\\*"
    "\\*Help\\*")
  "Regular expressions to determine if a buffer is useless.")

(defun bb-useful-buffer-p (buffer)
  "Determine if BUFFER is useful."
  (let ((name (buffer-name buffer)))
    (or (with-current-buffer buffer
          (derived-mode-p 'comint-mode))
        (cl-loop for regexp in bb-useful-buffers-regexp
                 thereis (string-match-p regexp name))
        (cl-loop for regexp in bb-useless-buffers-regexp
                 never (string-match-p regexp name)))))


;;; Evil-numbers

(defhydra hydra-numbers ()
  ("=" evil-numbers/inc-at-pt)
  ("+" evil-numbers/inc-at-pt)
  ("-" evil-numbers/dec-at-pt)
  ("_" evil-numbers/dec-at-pt))


;;; Evil-unimpaired

(defun bb-insert-line-above (count)
  "Insert COUNT lines above point."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun bb-insert-line-below (count)
  "Insert COUNT lines below point."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun bb-insert-spaces-before (count)
  "Insert COUNT spaces before point."
  (interactive "p")
  (dotimes (_ count) (insert " ")))

(defun bb-insert-spaces-after (count)
  "Insert COUNT spaces after point."
  (interactive "p")
  (forward-char)
  (dotimes (_ count) (insert " "))
  (backward-char (1+ count)))


;;; Evil other

(defun bb-shift-left ()
  (interactive)
  (call-interactively 'evil-shift-left)
  (execute-kbd-macro "gv"))

(defun bb-shift-right ()
  (interactive)
  (call-interactively 'evil-shift-right)
  (execute-kbd-macro "gv"))


;; Incremental fill paragraph (modified from @alphapapa)

(defvar bb--flex-fill-paragraph-column nil)

(bb-advise around fill-paragraph (&rest args)
  (let ((fill-column
         (setq bb--flex-fill-paragraph-column
               (if (equal last-command this-command)
                   (+ 5 (or bb--flex-fill-paragraph-column fill-column))
                 fill-column))))
    (apply orig-fn args)
    (message "Fill column: %s" fill-column)))


;;; Macrostep

(defhydra hydra-macrostep (:foreign-keys run)
  ("e" macrostep-expand)
  ("c" macrostep-collapse)
  ("<right>" macrostep-next-macro)
  ("<left>" macrostep-prev-macro)
  ("q" macrostep-collapse-all :exit t))


;;; Smartparens

(defun bb-sp-pair-newline-and-indent (&rest _)
  "Create an empty line between two delimiters."
  (save-excursion
    (newline)
    (indent-according-to-mode))
  (indent-according-to-mode))

(defmacro bb-apply-newline-indent (modes &rest pairs)
  "Apply newline and indent behaviour for all PAIRS in all MODES."
  `(progn
     ,@(cl-loop for pair in pairs
                collect `(sp-local-pair
                          ',modes
                          ,@(if (stringp pair) (list pair nil) pair) :post-handlers
                          '(:add (bb-sp-pair-newline-and-indent "RET"))))))


;;; Structured editing

(defun bb-wrap-paren ()
  "Wrap the symbol under point with parentheses."
  (interactive)
  (sp-wrap-with-pair "("))

(defhydra hydra-structured-editing-lisp ()
  ("u" undo-tree-undo)

  ("b" sp-forward-barf-sexp)
  ("B" sp-backward-barf-sexp)
  ("s" sp-forward-slurp-sexp)
  ("S" sp-backward-slurp-sexp)

  ("dd" sp-kill-sexp)
  ("ds" sp-kill-symbol)
  ("dw" sp-kill-word)

  ("w" bb-wrap-paren)

  ("h" sp-backward-symbol)
  ("<left>" sp-backward-sexp)
  ("l" sp-forward-symbol)
  ("<right>" sp-forward-sexp))


;;; Window management

(defun bb-shrink-window-horizontally (delta)
  "Shrink the current window horizontally by DELTA units."
  (interactive "p")
  (shrink-window delta 'horizontal))

(defun bb-shrink-window-vertically (delta)
  "Shrink the current window vertically by DELTA units."
  (interactive "p")
  (shrink-window delta nil))

(defun bb-enlarge-window-horizontally (delta)
  "Enlarge the current window horizontally by DELTA units."
  (interactive "p")
  (enlarge-window delta 'horizontal))

(defun bb-enlarge-window-vertically (delta)
  "Enlarge the current window vertically by DELTA units."
  (interactive "p")
  (enlarge-window delta nil))

(defhydra hydra-windows ()
  ("e" ace-select-window :exit t)

  ("h" windmove-left :exit t)
  ("j" windmove-down :exit t)
  ("k" windmove-up :exit t)
  ("l" windmove-right :exit t)

  ("H" windmove-left)
  ("J" windmove-down)
  ("K" windmove-up)
  ("L" windmove-right)

  ("s" evil-window-split)
  ("v" evil-window-vsplit)
  ("d" delete-window)
  ("(" bb-shrink-window-horizontally)
  ("[" bb-shrink-window-vertically)
  (")" bb-enlarge-window-horizontally)
  ("]" bb-enlarge-window-vertically)
  ("u" winner-undo)
  ("=" balance-windows-area :exit t)

  ("n" eyebrowse-create-window-config :exit t)
  ("w" eyebrowse-switch-to-window-config :exit t)
  ("r" eyebrowse-rename-window-config :exit t)
  ("C-j" eyebrowse-next-window-config)
  ("C-k" eyebrowse-prev-window-config)

  ("q" nil :exit t))


;;; VTerm

(defvar bb--vterms (make-hash-table :test 'equal))

(defun bb--get-or-create-vterm ()
  (let* ((root (or (projectile-project-root) "~/"))
         (buffer (ht-get bb--vterms root)))
    (when (and buffer (not (buffer-live-p buffer)))
      (ht-remove bb--vterms root)
      (setq buffer nil))
    (if buffer
        (switch-to-buffer buffer)
      (require 'vterm)
      (let ((default-directory root))
        (vterm))
      (rename-buffer (format "vterm: ~/%s" (file-relative-name root (getenv "HOME"))))
      (ht-set bb--vterms root (current-buffer)))))

(defun bb-vterm ()
  "Pop or hide a vterm."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (previous-buffer)
    (bb--get-or-create-vterm)))


;;; Evil shift width

(defvar bb-indent-vars
  '((latex-mode . LaTeX-indent-level))
  "Alist associating major modes to indent level variables.")

(defun bb-set-evil-shift-width ()
  (catch 'done
    (dolist (elt bb-indent-vars)
      (when (derived-mode-p (car elt))
        (setq-local evil-shift-width (symbol-value (cdr elt)))
        (throw 'done nil)))))


;;; Counsel, projectile, eyebrowse and bufler

(defvar bb-eyebrowse-plists
  (make-hash-table)
  "Hash table for storing extra data about eyebrowse workspaces.")

(defun bb-eyebrowse-plist-get (key &optional slot)
  "Get a property for an eyebrowse workspace."
  (unless slot (setq slot (eyebrowse--get 'current-slot)))
  (plist-get (ht-get bb-eyebrowse-plists slot) key))

(defun bb-eyebrowse-plist-set (key val &optional slot)
  "Set a property for an eyebrowse workspace."
  (unless slot (setq slot (eyebrowse--get 'current-slot)))
  (ht-set bb-eyebrowse-plists slot
          (plist-put (ht-get bb-eyebrowse-plists slot)
                     key val)))

(defun bb-bufler-eyebrowse-pre-switch ()
  "Before switching eyebrowse workspace, store the bufler path."
  (bb-eyebrowse-plist-set :bufler-path (frame-parameter nil 'bufler-workspace-path)))

(defun bb-bufler-eyebrowse-post-switch ()
  "After switching eyebrowse workspace, restore the bufler path."
  (bufler-workspace-frame-set (bb-eyebrowse-plist-get :bufler-path)))

(defun bb-bufler-workspace-frame-set-hook (path)
  "When the bufler path changes, set an eyebrowse workspace name."
  (when-let* ((project
               (catch 'done
                 (dolist (entry (reverse path))
                   (save-match-data
                     (when (and (stringp entry)
                                (or (string-match "\\`Projectile: \\(.*\\)\\'" entry)
                                    (string-match "\\`Dir: \\(.*\\)\\'" entry)))
                       (throw 'done (match-string 1 entry))))))))
    (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) project)))

(defun bb-bufler-workspace-frame-set ()
  "Set a bufler path based on best guess principles."
  (bufler-workspace-frame-set
   (butlast (bufler-group-tree-leaf-path (bufler-buffers) (current-buffer)))))

(defun bb-counsel-find-file-new-workspace-action (arg)
  "Action for `counsel-find-file' to open in a new workspace."
  (let ((eyebrowse-new-workspace t))
    (eyebrowse-create-window-config)
    (with-current-buffer (counsel-find-file-action arg)
      (bb-bufler-workspace-frame-set))))

(defun bb-counsel-projectile-new-workspace-action (arg)
  "Action for `counsel-projectile' to open in a new workspace."
  (let ((eyebrowse-new-workspace t)
        (prev-default-directory default-directory))
    (eyebrowse-create-window-config)
    (let ((default-directory prev-default-directory))
      (counsel-projectile-action arg))
    (bb-bufler-workspace-frame-set)))

(defun bb-counsel-recentf-new-workspace-action (arg)
  "Action for `counsel-recentf' to open in a new workspace."
  (let ((eyebrowse-new-workspace t))
    (eyebrowse-create-window-config)
    (with-current-buffer
        (with-ivy-window
          (find-file arg))
      (bb-bufler-workspace-frame-set))))

(defun bb-filter-bufler-workspace (candidates)
  "Filter candidates for `counsel-ibuffer' according to
`bufler-workspace-buffers'."
  (let ((active-buffers (bufler-workspace-buffers)))
    (cl-remove-if (lambda (c) (not (member (cdr c) active-buffers))) candidates)))

(defun bb-counsel-ibuffer (&optional all-p name)
  (interactive "P")
  (require 'counsel)
  (setq counsel-ibuffer--buffer-name (or name "*Ibuffer*"))
  (let ((candidates (if (or all-p (not (frame-parameter nil 'bufler-workspace-path)))
                        (bufler-buffer-alist-at nil)
                      (bb-filter-bufler-workspace (counsel-ibuffer--get-buffers)))))
    (ivy-read "Switch to buffer: " candidates
              :history 'counsel-ibuffer-history
              :action 'counsel-ibuffer-visit-buffer
              :caller 'counsel-ibuffer)))

;;; Miscellaneous

(defun bb-alternate-buffer ()
  "Switch to the previous buffer displayed in the current window."
  (interactive)
  (let ((buf (window-buffer)))
    (switch-to-buffer
     (cl-find-if (lambda (b) (not (eq b buf)))
                 (mapcar 'car (window-prev-buffers))))))

(defun bb-dimmer-predicate ()
  (string-prefix-p " *company-box-" (buffer-name)))

(defun bb-find-init ()
  "Open `user-init-file'."
  (interactive)
  (find-file user-init-file))

(defun bb-find-scratch ()
  "Open the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun bb-kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil))

(defun bb-kill-buffer-file ()
  "Kill the current buffer and delete its associated file, if any."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (y-or-n-p "Are you sure you want to delete this file? ")
        (delete-file filename 'trash)
        (kill-buffer buffer)))))

(defun bb-lsp-set-priority (server priority)
  (setf (lsp--client-priority (gethash server lsp-clients)) priority))

(defun bb-show-and-copy-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (when file-name
      (message (kill-new file-name)))))

(defun bb-latex-build (arg)
  "Build the document with latexmk.
If ARG is given, query for a command."
  (interactive "P")
  (if arg
      (TeX-command-master)
    (TeX-command "LatexMk" 'TeX-master-file nil)))

(defun bb-latex-check-compilation ()
  "View the output buffer if compiling.
If done compiling, kill the auxiliary buffer."
  (interactive)
  (cond
   (compilation-in-progress (TeX-recenter-output-buffer nil))
   (t (-when-let* ((buf (TeX-active-buffer)))
        (kill-buffer buf)))))

(defun bb-toggle-debug-on-error ()
  "Toggle the value of `debug-on-error'."
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "Debug on error now: %S" debug-on-error))

(defun bb-maybe-auto-fill-mode ()
  "Enable auto-fill mode except in certain major modes."
  (unless (derived-mode-p 'forge-post-mode)
    (auto-fill-mode)))

(defun bb-compilation-filter ()
  "Filter and apply ANSI sequences in compilation output."
  (read-only-mode 'toggle)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 'toggle)
  )

(defun bb-ivy-posframe-display-frame-top-center (str)
  (ivy-posframe--display str #'posframe-poshandler-frame-top-center))

(provide 'bb-defs)

;;; bb-defs.el ends here
