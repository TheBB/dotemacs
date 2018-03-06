;;; -*- lexical-binding: t -*-

(setq bb-cfg-dir (file-name-directory load-file-name)
      package-enable-at-startup nil
      custom-file (concat bb-cfg-dir "custom.el")
      tool-bar-mode nil
      menu-bar-mode nil)

(set-scroll-bar-mode nil)

(push (concat bb-cfg-dir "lib") load-path)
(push (concat bb-cfg-dir "config") load-path)
