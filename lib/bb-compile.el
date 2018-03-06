;;; -*- lexical-binding: t -*-

(require 'bytecomp)
(require 'package)

(defvar bb--cfg nil)
(defvar bb-packages nil)
(defvar bb-normalized-packages nil)

(defun bb--normalize-packages ()
  (setq bb-normalized-packages
	(mapcar (lambda (pkg)
		  (let* ((pkg (if (consp pkg) pkg (list pkg)))
			 (loc (or (plist-get (cdr pkg) :location) 'remote)))
		    (when (stringp loc)
		      (setq loc (expand-file-name loc)))
		    `(,(car pkg) :location ,loc)))
		bb-packages)))

(defun bb-normalized-packages ()
  (unless bb-normalized-packages (bb--normalize-packages))
  bb-normalized-packages)

(defun bb-pkg-location (pkg)
   (plist-get (cdr pkg) :location))

(defun bb--get-cfg (pkg-name stage)
  "Gets the configuration code for the package named PKG-NAME to
be executed at boot-stage STAGE. Returns a list of forms."
  (cdr (assq stage (cdr (assq pkg-name bb--cfg)))))

(defun bb--mplist-get (plist prop)
  "Extracts the value associated with the key PROP in the
multi-valued property list PLIST."
  (let (result)
    (while (and (consp plist) (not (eq prop (car plist))))
      (pop plist))
    (pop plist)
    (while (and (consp plist) (not (keywordp (car plist))))
      (push (pop plist) result))
    (nreverse result)))

(defmacro bb-package (name &rest code)
  "A do-nothing macro that stores code to be compiled into the
init file. Don't use outside package config files."
  (declare (indent 1))
  (push `(,name . ((pre-init . ,(bb--mplist-get code :pre-init))
		   (init . ,(bb--mplist-get code :init))
		   (post-init . ,(bb--mplist-get code :post-init))))
	bb--cfg)
  `(provide ',(intern (format "bb-%s" name))))

(defmacro bb-set-load-path ()
  "Set the load path for all installed packages. Also load their
autoloads files."
  (let ((ignore '("." ".." "archives" "gnupg"))
	paths autoloads)
    (dolist (pkg-name (directory-files package-user-dir))
      (unless (member pkg-name ignore)
	(let ((full-pkg-dir (concat package-user-dir pkg-name)))
	  (push `(push ,full-pkg-dir load-path) paths)
	  (dolist (file (directory-files full-pkg-dir))
	    (when (string-match "autoloads\\.el\\'" file)
	      (push `(load-file ,(concat full-pkg-dir "/" file)) autoloads))))))
    (dolist (pkg (bb-normalized-packages))
      (when (stringp (bb-pkg-location pkg))
	(push `(push ,(bb-pkg-location pkg) load-path) paths)))
    `(progn ,@paths ,@autoloads)))

(defmacro bb-stage (stage)
  "Run initialization code for boot-stage STAGE."
  (let (code)
    (dolist (pkg (reverse (bb-normalized-packages)))
      (setq code (append (bb--get-cfg (car pkg) stage) code)))
    `(progn ,@code)))

(defun bb-compile ()
  "Recompile the init file."
  (interactive)
  (dolist (pkg (bb-normalized-packages))
    (require (intern (format "bb-%s" (car pkg))) nil 'noerror))
  (dolist (dir '())
    (byte-recompile-directory (concat bb-cfg-dir dir) 0 'force))
  (dolist (file '("early-init.el" "init.el"))
    (byte-recompile-file (concat bb-cfg-dir file) 'force 0)))

(defun bb-update ()
  "Update packages."
  (interactive)
  (package-initialize)
  (package-refresh-contents)
  (dolist (pkg (bb-normalized-packages))
    (when (eq 'remote (bb-pkg-location pkg))
      (package-install (car pkg) nil))))

(provide 'bb-compile)
