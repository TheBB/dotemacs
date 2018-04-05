;;; bb-hooks.el --- Personal hook functions. -*- lexical-binding: t -*-

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

;; Major mode hook functions needed in my Emacs init.

;;; Code:


(defun bb-evil-insert-state-exit ()
  "Run when exiting insert-state."
  (deactivate-mark))

(defun bb-elisp ()
  "Run in `emacs-lisp-mode'."
  (push '("Package" "\\(^\\s-*(use-package +\\)\\(\\_<[^ ]+\\_>\\)" 2) imenu-generic-expression))

(defun bb-erc ()
  "Run in `erc-mode'."
  (setq-local global-hl-line-mode nil))


(provide 'bb-hooks)

;; Local variables:
;; eval: (auto-compile-mode)
;; End:

;;; bb-hooks.el ends here
