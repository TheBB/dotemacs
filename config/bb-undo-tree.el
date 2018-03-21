(bb-package undo-tree

  :init
  (setq undo-tree-enable-undo-in-region nil)
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode)
  (bb-leader "au" 'undo-tree-visualize)

  :post-init popwin
  (bb-popwin undo-tree-visualizer-mode :width 60 :position right))
