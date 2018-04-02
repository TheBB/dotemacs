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
  (require 'hydra))



;; Postpone code until after display is initialized

(defvar bb--after-display-functions nil
  "List of functions to be run (in reverse order) after the
display system is initialized.")

(defun bb--server-create-window-system-frame (&rest args)
  (dolist (func (reverse bb--after-display-functions))
    (funcall func))
  (advice-remove 'server-create-window-system-frame 'bb--server-create-window-system-frame))

(advice-add 'server-create-window-system-frame :after 'bb--server-create-window-system-frame)


(defmacro bb-after-display (&rest body)
  (declare (indent 0))
  "Run BODY after the display system is initialized."
  `(let ((initializedp (cond ((boundp 'ns-initialized) ns-initialized)
                             ((boundp 'w32-initialized) (font-family-list))
                             ((boundp 'x-initialized) x-initialized)
                             (t (display-graphic-p)))))
     (if initializedp
         (progn ,@body)
       (push (lambda () ,@body) bb--after-display-functions))))



;; Convenience functions for leader bindings

(declare-function 'general-define-key "general")

(defmacro bb-leader (&rest args)
  "Bind ARGS as leader bindings."
  (declare (indent 0))
  `(progn
     (require 'general)
     (general-define-key :prefix "SPC" :states '(normal motion) :keymaps 'override ,@args)))

(defmacro bb-create-dispatch (keys)
  "Generate a major mode dispatch system for KEYS."
  (let ((funcname (intern (format "bb-dispatch-%s" keys)))
        (varname (intern (format "bb-dispatch-table-%s" keys))))
    `(progn
       (unless (boundp ',varname)
         (defvar ,varname nil)
         (defun ,funcname ()
           (interactive)
           (cl-loop for entry in ,varname
                    if (derived-mode-p (car entry))
                    return (call-interactively (cdr entry))
                    finally return (user-error "No dispatch found for \"%s\" in %s" ,keys major-mode)))
         (bb-leader ,keys ',funcname)))))

(defmacro bb-mm-leader (mode &rest args)
  "Bind ARGS in MODE as leader bindings."
  (declare (indent 1))
  (let (bindings)
    (while args
      (push (cons (car args) (cadr args)) bindings)
      (setq args (cddr args)))
    `(progn
       ,@(cl-loop for binding in bindings
                  collect `(bb-create-dispatch ,(car binding)))
       ,@(cl-loop for binding in bindings
                  collect `(push (cons ',mode ,(cdr binding))
                                 ,(intern (format "bb-dispatch-table-%s" (car binding))))))))



;; Predefined window configurations

(defvar bb--display-index 100
  "Internal counter used in `bb-define-display.'")

(defmacro bb-define-display (name leader &rest kwargs)
  (declare (indent 2))
  (let ((funcname (intern (format "bb--display-%s" name)))
        (flagname (intern (format "bb--display-%s-ready" name)))
        (index (cl-incf bb--display-index))
        (layout (plist-get kwargs :layout))
        (startup (plist-get kwargs :startup))
        (buffers (plist-get kwargs :buffers)))
    `(progn
       (defvar ,flagname nil)
       (defun ,funcname ()
         (interactive)
         (let ((existsp (eyebrowse--window-config-present-p ,index)))
           (eyebrowse-switch-to-window-config ,index)
           (unless ,flagname
             ,startup
             (setq ,flagname t))
           (unless existsp
             (eyebrowse-rename-window-config ,index ,name)
             ,@(when layout `((purpose-load-window-layout ,layout))))
           ,@(when buffers
               `((dolist ((win (window-list)))
                   (set-window-buffer win ,(pop buffers)))))))
       (bb-leader ,leader ',funcname))))



;; Buffer predicate function

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



;; Company

(defvar bb-company-global-backends nil
  "List of backends to enable everywhere.")

(defmacro bb-company (mode &rest backends)
  "Run `company-mode' in MODE with BACKENDS."
  (declare (indent 1))
  (let ((funcname (intern (format "bb-company-%s" mode)))
        (hookname (intern (format "%s-hook" mode))))
    `(progn
       (defun ,funcname ()
         (company-mode)
         (setq-local company-backends
                     (list (append ',backends bb-company-global-backends))))
       (add-hook ',hookname ',funcname))))



;; Evil-numbers

(defhydra hydra-numbers ()
  ("=" evil-numbers/inc-at-pt)
  ("+" evil-numbers/inc-at-pt)
  ("-" evil-numbers/dec-at-pt)
  ("_" evil-numbers/dec-at-pt))



;; Evil-unimpaired

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



;; Functions to remove dotted entries from `helm-find-files'

(defun bb-helm-ff-filter-candidate-one-by-one (func file)
  "Filter out '.' and '..' from FILE.  Otherwise call FUNC."
  (unless (string-match-p "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)
    (funcall func file)))

(defun bb-helm-file-completion-source-p (&rest _)
  "Always return true."
  t)

(defun bb-helm-attrset (func attribute-name value &optional src)
  "Wrapper for FUNC that ensures SRC is always a valid helm source.
For ATTRIBUTE-NAME and VALUE, see `helm-attrset'."
  (let ((src (or src (helm-get-current-source))))
    (when src (funcall func attribute-name value src))))

(defun bb-helm-find-files-up-one-level (func &rest args)
  "Wrapper for FUNC that works without looking at any entries.
For ARGS, see `helm-find-files'."
  (advice-add 'helm-file-completion-source-p :around 'bb-helm-file-completion-source-p)
  (advice-add 'helm-attrset :around 'bb-helm-attrset)
  (let ((res (apply func args)))
    (advice-remove 'helm-file-completion-source-p 'bb-helm-file-completion-source-p)
    (advice-remove 'helm-attrset 'bb-helm-attrset)
    res))



;; Show the helm buffer in a child frame
;; Lifted with modifications from:
;;   https://gist.github.com/fuxialexander/5ad46671689d96a29f9865c1c0b42d10

(defvar bb-helm--frame-alist
  '((undecorated . t)
    (left-fringe . 0)
    (right-fringe . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (desktop-dont-save . t)
    (no-special-glyphs . t)
    (inhibit-double-buffering . t)
    (tool-bar-lines . 0)
    (title . "Helm")
    (vertical-scroll-bars . nil)
    (menu-bar-lines . nil)
    (fullscreen . nil)
    (minibuffer . t)
    (alpha . 90))
  "Frame parameters that apply to all helm child frames.")

(defun bb-helm-display-child-frame (buffer &optional _)
  "Display the helm buffer BUFFER in a child frame.
Suitable for `helm-display-function'."

  ;; Fallback to regular display if not in a GUI
  (if (not (display-graphic-p))
      (helm-default-display-buffer buffer)
    (setq helm--buffer-in-new-frame-p t)
    (let* ((pos (window-absolute-pixel-position))
           (char-size (cons (frame-char-width) (frame-char-height)))
           (frame-info (frame-geometry))

           ;; The right and bottom boundaries are given by the
           ;; parent frame's right border and the bottom of the screen
           (parent-right (+ (cadr (assq 'outer-position frame-info))
                            (cadr (assq 'outer-size frame-info))))
           (parent-bottom (display-pixel-height x-display-name))
           (helm-pixel-width (* (car char-size) helm-display-buffer-width))
           (helm-pixel-height (* (cdr char-size) helm-display-buffer-height))

           ;; Show helm above the cursor if there's not enough space below
           (abovep (> (+ (cdr pos) (cdr char-size) helm-pixel-height)
                      parent-bottom))

           ;; Calculate the positions of the child frame.
           (left (max 0 (min (car pos) (- parent-right helm-pixel-width))))
           (top (if abovep
                    (- (cdr pos) helm-pixel-height)
                  (+ (cdr pos) (cdr char-size))))

           ;; Finalize the frame parameters
           (default-frame-alist (append bb-helm--frame-alist
                                        `((parent . ,(selected-frame))
                                          (width . ,helm-display-buffer-width)
                                          (height . ,helm-display-buffer-height)
                                          (left . ,left)
                                          (top . ,top)
                                          (visible . ,(null helm-display-buffer-reuse-frame)))))
           display-buffer-alist)
      (with-helm-buffer (setq-local helm-echo-input-in-header-line (not abovep)))
      (helm-display-buffer-popup-frame buffer default-frame-alist))
    (helm-log-run-hook 'helm-window-configuration-hook)))



;; Helm helpers

(defun bb-helm-ag-project ()
  "Call `helm-do-ag' in the project root."
  (interactive)
  (helm-do-ag (projectile-project-root)))

(defun bb-helm-swoop ()
  "Call `helm-swoop' with `helm-echo-input-in-header-line' set to true."
  (interactive)
  (let ((helm-echo-input-in-header-line t))
    (call-interactively 'helm-swoop)))



;; LSP

(declare-function 'lsp-ui-imenu-enable "lsp-ui-imenu")
(declare-function 'lsp-ui-flycheck-add-mode "lsp-ui-flycheck")
(defvar flycheck-checker)
(defvar flycheck-checkers)

(defun bb-lsp-enable-ui ()
  "Enable LSP user interface tools."
  (lsp-ui-imenu-enable t)
  (setq-local flycheck-checker 'lsp-ui)
  (lsp-ui-flycheck-add-mode major-mode)
  (add-to-list 'flycheck-checkers 'lsp-ui))



;; Macrostep

(defhydra hydra-macrostep (:foreign-keys run)
  ("e" macrostep-expand)
  ("c" macrostep-collapse)
  ("<right>" macrostep-next-macro)
  ("<left>" macrostep-prev-macro)
  ("q" macrostep-collapse-all :exit t))



;; Smartparens

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



;; Structured editing

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



;; Window management

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
  ("a" ace-select-window :exit t)
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
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
  ("C-j" eyebrowse-next-window-config)
  ("C-k" eyebrowse-prev-window-config)

  ("q" nil :exit t))



;; Miscellaneous

(defmacro bb-popwin (mode)
  "Push (MODE ARGS...) to `popwin:special-display-config'."
  `(if (featurep 'window-purpose-x)
       (progn
         (push ',mode purpose-x-popwin-major-modes)
         (purpose-x-popwin-update-conf))
     (with-eval-after-load 'window-purpose-x
       (push ',mode purpose-x-popwin-major-modes))))

(defmacro bb-adv-only-in-modes (func &rest modes)
  "Advice FUNC only to run then `major-mode' is exactly any of MODES."
  (declare (indent 1))
  (let ((funcname
         (intern (format "bb--only-in-modes-%s" (mapconcat 'symbol-name modes "-or-")))))
    `(progn
       (defun ,funcname (orig-fn &rest args)
         (when (or ,@(cl-loop for mode in modes collect `(eq major-mode ',mode)))
           (apply orig-fn args)))
       (advice-add ',func :around ',funcname))))

(defmacro bb-adv-except-derived-modes (func &rest modes)
  "Advice FUNC only to run when `major-mode' is derived from any of MODES."
  (declare (indent 1))
  (let ((funcname
         (intern (format "bb--except-derived-modes-%s" (mapconcat 'symbol-name modes "-or-")))))
    `(progn
       (defun ,funcname (orig-fn &rest args)
         (unless (derived-mode-p ,@(cl-loop for mode in modes collect `(quote ,mode)))
           (apply orig-fn args)))
       (advice-add ',func :around ',funcname))))

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

(provide 'bb-defs)

;; Local variables:
;; eval: (auto-compile-mode)
;; End:

;;; bb-defs.el ends here
