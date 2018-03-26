(eval-when-compile
  (require 'helm))



;; Configuration source

(defun bb-helm-new-config-file (filename)
  (find-file (concat (bb-dir "config/bb-") filename ".el")))

(defun bb-helm-config ()
  (interactive)
  (require 'helm)
  (require 'helm-source)
  (helm :buffer "*helm: config*"
        :sources (list (helm-build-sync-source "Configuration files"
                         :candidates 'bb-helm-config-candidates
                         :persistent-action 'find-file
                         :action 'find-file
                         :fuzzy-match t)
                       (helm-build-dummy-source "New configuration file"
                         :persistent-action 'bb-helm-new-config-file
                         :action 'bb-helm-new-config-file))))

(defun bb-helm-config-candidates ()
  (cl-loop for f in (directory-files (bb-dir "config"))
           unless (string-match-p f "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'")
           collect (cons (substring f 3 -3) (concat (bb-dir "config/") f))))



;; Remove dotted entries from helm-find-files

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
