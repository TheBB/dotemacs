(eval-when-compile
  (require 'cl-lib)
  (require 'hydra))



;; Convenience functions for leader bindings

(declare-function 'general-define-key "general")

(defmacro bb-leader (&rest args)
  (declare (indent 0))
  `(progn
     (require 'general)
     (general-define-key :prefix "SPC" :states '(normal motion) :keymaps 'override ,@args)))

(defmacro bb-create-dispatch (keys)
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



;; Buffer predicate function

(defvar bb-useful-buffers-regexp nil)

(defvar bb-useless-buffers-regexp
  '("\\*Messages\\*"
    "\\*Help\\*"))

(defun bb-useful-buffer-p (buffer)
  "Determine if a buffer is useful."
  (let ((name (buffer-name buffer)))
    (or (with-current-buffer buffer
          (derived-mode-p 'comint-mode))
        (cl-loop for regexp in bb-useful-buffers-regexp
                 thereis (string-match-p regexp name))
        (cl-loop for regexp in bb-useless-buffers-regexp
                 never (string-match-p regexp name)))))



;; Company

(defvar bb-company-global-backends nil)

(defmacro bb-company (mode &rest backends)
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
  (interactive "p")
  (dotimes (- count) (save-excursion (evil-insert-newline-above))))

(defun bb-insert-line-below (count)
  (interactive "p")
  (dotimes (- count) (save-excursion (evil-insert-newline-below))))

(defun bb-insert-spaces-before (count)
  (interactive "p")
  (dotimes (- count) (insert " ")))

(defun bb-insert-spaces-after (count)
  (interactive "p")
  (forward-char)
  (dotimes (- count) (insert " "))
  (backward-char (1+ count)))



;; LSP

(declare-function 'lsp-ui-imenu-enable "lsp-ui-imenu")
(declare-function 'lsp-ui-flycheck-add-mode "lsp-ui-flycheck")
(defvar flycheck-checker)
(defvar flycheck-checkers)

(defun bb-lsp-enable-ui ()
  (lsp-ui-imenu-enable t)
  (setq-local flycheck-checker 'lsp-ui)
  (lsp-ui-flycheck-add-mode major-mode)
  (add-to-list 'flycheck-checkers 'lsp-ui))



;; Smartparens

(defun bb-sp-pair-newline-and-indent (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode))
  (indent-according-to-mode))

(defmacro bb-apply-newline-indent (modes &rest pairs)
  `(progn
     ,@(cl-loop for pair in pairs
                collect `(sp-local-pair ',modes ,pair nil :post-handlers
                                        '(:add (bb-sp-pair-newline-and-indent "RET"))))))



;; Window management

(defun bb-shrink-window-horizontally (delta)
  (interactive "p")
  (shrink-window delta 'horizontal))

(defun bb-shrink-window-vertically (delta)
  (interactive "p")
  (shrink-window delta nil))

(defun bb-enlarge-window-horizontally (delta)
  (interactive "p")
  (enlarge-window delta 'horizontal))

(defun bb-enlarge-window-vertically (delta)
  (interactive "p")
  (enlarge-window delta nil))

(defhydra hydra-windows ()
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
  ("=" balance-windows-area :exit t))



;; Miscellaneous

(defvar popwin:special-display-config)

(defmacro bb-popwin (mode &rest args)
  `(push '(,mode ,@args) popwin:special-display-config))

(defmacro bb-adv-only-in-modes (func &rest modes)
  (let ((funcname
         (intern (format "bb--only-in-modes-%s" (mapconcat 'symbol-name modes "-or-")))))
    `(progn
       (defun ,funcname (orig-fn &rest args)
         (when (or ,@(cl-loop for mode in modes collect `(eq major-mode ',mode)))
           (apply orig-fn args)))
       (advice-add ',func :around ',funcname))))

(defun bb-alternate-buffer ()
  (interactive)
  (let ((buf (window-buffer)))
    (switch-to-buffer
     (cl-find-if (lambda (b) (not (eq b buf)))
                 (mapcar 'car (window-prev-buffers))))))

(defun bb-kill-buffer ()
  (interactive)
  (kill-buffer nil))

(defun bb-kill-buffer-file ()
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (y-or-n-p "Are you sure you want to delete this file? ")
        (delete-file filename 'trash)
        (kill-buffer buffer)))))

(provide 'bb-defs)
