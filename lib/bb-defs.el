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



;; Functions to remove dotted entries from `helm-find-files'

(defun bb-helm-ff-filter-candidate-one-by-one (func file)
  (unless (string-match-p "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)
    (funcall func file)))

(defun bb-helm-file-completion-source-p (&rest args) t)

(defun bb-helm-attrset (func attribute-name value &optional src)
  (let ((src (or src (helm-get-current-source))))
    (when src (funcall func attribute-name value src))))

(defun bb-helm-find-files-up-one-level (func &rest args)
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

(defun bb-helm-display-child-frame (buffer &optional resume)
  "Display helm in a child frame. Suitable for
`helm-display-function'."

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
  (interactive)
  (helm-do-ag (projectile-project-root)))

(defun bb-helm-swoop ()
  (interactive)
  (let ((helm-echo-input-in-header-line t))
    (call-interactively 'helm-swoop)))



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
