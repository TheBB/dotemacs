;;; -*- lexical-binding: t -*-

;; The load path must be set when compiling to get access to macros
(eval-when-compile
  (require 'bb-compile)
  (bb-set-load-path))

;; Set the load path during startup, too
(bb-set-load-path)

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
