;;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'bb-compile)
  (bb-set-load-path))

(bb-set-load-path)
(bb-stage pre-init)
(bb-stage init)
(bb-stage post-init)

(load custom-file)
