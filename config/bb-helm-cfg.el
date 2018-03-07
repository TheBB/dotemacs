(eval-when-compile
  (require 'helm))


;; Show the helm buffer in a child frame
;; Lifted with modifications form:
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
           (left (min (car pos) (- parent-right helm-pixel-width)))
           (top (if abovep
                    (- (cdr pos) helm-pixel-height)
                  (+ (cdr pos) (cdr char-size))))

           ;; Finalize the frame parmeters
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
