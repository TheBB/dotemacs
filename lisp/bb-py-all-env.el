;;; bb-defs.el --- Code for switching between any kind of Python environment. -*- lexical-binding: t -*-

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

;; Wraps pyvenv and conda in one sweet package.

;;; Code:

(require 'pyvenv nil t)
(require 'conda nil t)

(declare-function pyenv-mode-version "ext:pyenv-mode")

(defmacro bb-py-all-env-annotate (name func list)
  (declare (indent 2))
  `(mapcar (lambda (c)
             (cons (format "(%s) %s" ',name c)
                   (list ',func c)))
           ,list))

(defun bb-py-all-env-list ()
  (append
   (bb-py-all-env-annotate pyvenv pyvenv-workon
     (and (fboundp 'pyvenv-virtualenv-list) (pyvenv-virtualenv-list 'noerror)))
   (bb-py-all-env-annotate conda conda-env-activate
     (and (fboundp 'conda-env-candidates) (conda-env-candidates)))))

(defun bb-py-all-env-activate ()
  (interactive)
  (let* ((candidates (bb-py-all-env-list))
         (env (completing-read "Environment: " candidates nil t)))
    (eval (cdr (assoc env candidates)))))

(defun bb-py-all-env-deactivate ()
  (interactive)
  (cond
   ((bound-and-true-p pyvenv-virtual-env-name)
    (pyvenv-deactivate))
   ((bound-and-true-p conda-env-current-name)
    (conda-env-deactivate))))

(defun bb-py-all-env-parse (line)
  "Return version and venv info.
Useful as parsing function for `doom-modeline-env'."
  (let (name source (version (cadr (split-string line))))
    (cond
     ((bound-and-true-p pyvenv-virtual-env-name)
      (setq name pyvenv-virtual-env-name source "pyvenv"))
     ((bound-and-true-p conda-env-current-name)
      (setq name conda-env-current-name source "conda"))
     ((and (fboundp 'pyenv-mode) (setq name (pyenv-mode-version)))
      (setq source "pyenv")))
    (if name
        (format "%s %s" version
                (propertize name 'help-echo (format "Virtual environment (via %s)" source)))
      version)))


(provide 'bb-py-all-env)

;;; bb-py-all-env.el ends here
