;;; bb-erc.el --- Personal ERC functions. -*- lexical-binding: t -*-

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

;; Miscellaneous functions needed for ERC in my Emacs init.

;;; Code:

(require 'erc)

(defvar bb-znc-pwd nil)

(defun bb-erc ()
  "Start `erc'."
  (interactive)
  (if bb-znc-pwd
      (erc :server "efonn.no"
           :port 1025
           :nick "TheBB"
           :password (format "TheBB/freenode:%s" bb-znc-pwd))
    (user-error "Missing ZNC password")))

(provide 'bb-erc)

;;; bb-erc.el ends here
