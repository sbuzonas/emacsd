(defaddon undo-tree
  "Replace standard undo behavior with a graphical tree representation."
  (fg/require-package 'undo-tree)

  (require 'undo-tree)
  (global-undo-tree-mode))
