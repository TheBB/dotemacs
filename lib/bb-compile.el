;;; -*- lexical-binding: t -*-

(require 'bytecomp)
(require 'package)

(defvar bb--cfg nil)
(defvar bb-packages nil)
(defvar bb-normalized-packages nil)

(defun bb--normalize-packages ()
  "Normalize the package specs by filling in default values where
appropriate."
  (setq bb-normalized-packages
	(mapcar (lambda (pkg)
		  (let* ((pkg (if (consp pkg) pkg (list pkg)))
			 (loc (or (plist-get (cdr pkg) :location) 'remote)))
		    (when (stringp loc)
		      (setq loc (expand-file-name loc)))
		    `(,(car pkg) :location ,loc)))
		bb-packages)))

(defun bb-normalized-packages ()
  "Return the list of normalized package specs."
  (unless bb-normalized-packages (bb--normalize-packages))
  bb-normalized-packages)

(defun bb-pkg-location (pkg)
  "Get the location property from a package plist PKG."
  (plist-get (cdr pkg) :location))

(defun bb--get-cfg-alist (pkg-name)
  "Get the configuration alist for the package PKG-NAME. This is
an alist mapping stage names to lists of forms. If the alist
doesn't exist it is created first."
  (unless (assq pkg-name bb--cfg)
    (push (cons pkg-name (list (list 'cfg)
                               (list 'boot)
                               (list 'pre-init)
                               (list 'init)
                               (list 'post-init)))
          bb--cfg))
  (cdr (assq pkg-name bb--cfg)))

(defun bb--get-cfg (pkg-name stage)
  "Gets the configuration code for the package named PKG-NAME to
be executed at boot-stage STAGE. Returns a list of forms."
  (cdr (assq stage (bb--get-cfg-alist pkg-name))))

(defun bb-push-cfg (pkg-name stage forms)
  "Add the forms FORMS to be executed during the
initialization-stage STAGE for the package PKG-NAME."
  (let ((cons (assq stage (bb--get-cfg-alist pkg-name))))
    (setcdr cons (append (cdr cons) forms))))

(defun bb--copy-to-kw (plist)
  "Copies the list PLIST up to (and not including) the first
keyword element found."
  (let (result)
    (while (and (consp plist) (not (keywordp (car plist))))
      (push (pop plist) result))
    (nreverse result)))

(defmacro bb-dir (dir)
  "Macro for statically resolving a subdirectory of the root
dir."
  (concat bb-cfg-dir dir))

(defmacro bb-autoload (file &rest funcs)
  (declare (indent 1))
  `(progn
     ,@(cl-loop for func in funcs
                collect `(autoload ',func ,file))))

(defmacro bb-package (name &rest code)
  "A do-nothing macro that stores code to be compiled into the
init file. Don't use outside package config files."
  (declare (indent 1))
  (let (forms temp)
    (while code
      (setq temp (bb--copy-to-kw (cdr code)))
      (cond
       ((eq (car code) :boot) (push `(bb-push-cfg ',name 'boot ',temp) forms))
       ((eq (car code) :init) (push `(bb-push-cfg ',name 'init ',temp) forms))
       ((eq (car code) :pre-init) (push `(bb-push-cfg ',(car temp) 'pre-init ',(cdr temp)) forms))
       ((eq (car code) :post-init) (push `(bb-push-cfg ',(car temp) 'post-init ',(cdr temp)) forms)))
      (pop code) 
      (while (and code (not (keywordp (car code)))) (pop code)))
    `(progn
       ,@forms
       (provide ',(intern (format "bb-%s" name))))))

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
    (let ((cfg-file (format "%sconfig/bb-%s-cfg.el" bb-cfg-dir (car pkg))))
      (when (file-exists-p cfg-file)
        (with-temp-buffer
          (insert-file-contents cfg-file)
          (bb-push-cfg (car pkg) 'cfg
                       (cdr (read (format "(progn\n%s)\n" (buffer-string))))))))
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
