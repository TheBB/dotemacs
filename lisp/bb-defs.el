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
  (require 'bb-macros))


;;; Executables

(setq bb-executables
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
     (version-regexp . "LanguageServer v\\([0-9\\.+]*\\)"))))

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
  (dotimes (- count) (save-excursion (evil-insert-newline-above))))

(defun bb-insert-line-below (count)
  "Insert COUNT lines below point."
  (interactive "p")
  (dotimes (- count) (save-excursion (evil-insert-newline-below))))

(defun bb-insert-spaces-before (count)
  "Insert COUNT spaces before point."
  (interactive "p")
  (dotimes (- count) (insert " ")))

(defun bb-insert-spaces-after (count)
  "Insert COUNT spaces after point."
  (interactive "p")
  (forward-char)
  (dotimes (- count) (insert " "))
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
                collect `(sp-local-pair ',modes ,pair nil :post-handlers
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


;;; Miscellaneous

(defun bb-alternate-buffer ()
  "Switch to the previous buffer displayed in the current window."
  (interactive)
  (let ((buf (window-buffer)))
    (switch-to-buffer
     (cl-find-if (lambda (b) (not (eq b buf)))
                 (mapcar 'car (window-prev-buffers))))))

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

(defun bb-vterm (&optional renew)
  "Pop or hide a vterm."
  (interactive "P")
  (require 'vterm)
  (when renew
    (let ((kill-buffer-query-functions
           (remove 'process-kill-buffer-query-function
                   kill-buffer-query-functions))))
    (ignore-errors (kill-buffer "vterm")))
  (if (derived-mode-p 'vterm-mode)
      (previous-buffer)
    (if-let* ((buffer (get-buffer "vterm")))
        (switch-to-buffer buffer)
      (vterm))))

(defun bb-maybe-auto-fill-mode ()
  "Enable auto-fill mode except in certain major modes."
  (unless (derived-mode-p 'forge-post-mode)
    (auto-fill-mode)))

(defun bb-compilation-filter ()
  "Filter and apply ANSI sequences in compilation output."
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(provide 'bb-defs)

;;; bb-defs.el ends here
