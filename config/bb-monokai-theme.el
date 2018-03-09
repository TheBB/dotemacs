(bb-package monokai-theme
  :boot
  (load-theme 'monokai 'noconfirm)

  ;; General theming
  (set-face-attribute 'default nil :font "Iosevka Expanded Bold" :height 100)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-string-face nil :slant 'italic)
  (set-face-attribute 'font-lock-doc-face nil :slant 'italic :foreground "#75715e")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "#ff4185" :weight 'bold)
  (set-face-attribute 'font-lock-builtin-face nil :foreground "#ffabd6" :weight 'bold)

  ;; Modeline
  (set-face-attribute 'header-line nil :box '(:color "#555555"))
  (set-face-attribute 'mode-line nil :box '(:color "#999999" :line-width 1 :style released-button))
  (set-face-attribute 'mode-line-inactive nil :box '(:color "#666666" :line-width 1 :style released-button))
  (with-eval-after-load 'powerline
    (set-face-attribute 'powerline-active1 nil
                        :box '(:color "#999999" :line-width 1 :style released-button)
                        :background "#5a5a5a")
    (set-face-attribute 'powerline-active2 nil
                        :box '(:color "#999999" :line-width 1 :style released-button))
    (set-face-attribute 'powerline-inactive1 nil
                        :box '(:color "#666666" :line-width 1 :style released-button))
    (set-face-attribute 'powerline-inactive2 nil
                        :box '(:color "#666666" :line-width 1 :style released-button)))
  (with-eval-after-load 'helm
    (set-face-attribute 'helm-prefarg nil :foreground "PaleGreen")))
