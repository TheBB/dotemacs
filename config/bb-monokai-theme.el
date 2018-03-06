(bb-package monokai-theme
  :pre-init
  (load-theme 'monokai 'noconfirm)
  (set-face-attribute 'default nil :font "Iosevka Expanded Bold" :height 100)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-string-face nil :slant 'italic)
  (set-face-attribute 'font-lock-doc-face nil :slant 'italic :foreground "#75715e")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "#ff4185" :weight 'bold)
  (set-face-attribute 'font-lock-builtin-face nil :foreground "#ffabd6" :weight 'bold))
