;;; -*- lexical-binding: t -*-

(setq bb-cfg-dir (file-name-directory load-file-name))
(setq custom-file (concat bb-cfg-dir "custom.el"))

;; The load path must be set when compiling to get access to macros
(eval-when-compile
  (push (concat bb-cfg-dir "lib") load-path)
  (push (concat bb-cfg-dir "config") load-path)
  (require 'bb-compile)
  (load (concat bb-cfg-dir "config.el"))
  (bb-set-load-path)
  (bb-collect-cfg))

;; Set the load path during startup, too
(bb-set-load-path)

;; Planning to migrate this to a borg collective eventually
(require 'borg)
(borg-initialize)

;; The configuration stage runs code from all bb-PKG-cfg.el files
;; Generally intended for defuns and defvars
(bb-stage cfg)

;; The boot stage runs code that needs to happen as soon as Emacs boots
;; Maybe this should go in early-init?
(bb-stage boot)

;; Code injected from other packages
(bb-stage pre-init)

;; Actual configuration code
(bb-stage init)

;; Code injected from other packages
(bb-stage post-init)

;; Finally load customizations, if any
(load custom-file)
