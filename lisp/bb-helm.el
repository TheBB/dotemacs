;;; bb-helm.el --- Helm-specific functions. -*- lexical-binding: t -*-

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

;; Miscellaneous functions for helm.

;;; Code:


(require 'helm)

(declare-function projectile-project-root "projectile")



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


(provide 'bb-helm)

;;; bb-helm.el ends here
