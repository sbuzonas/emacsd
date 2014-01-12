(defcustom addon-guide-key
  nil
  "Enable guide-key features"
  :type 'boolean
  :group 'features)

(when addon-guide-key
  (progn
    (add-to-list 'my-default-packages 'undo-tree)
    (after 'undo-tree
      (global-undo-tree-mode)
      (setq undo-tree-mode-lighter "")
      (setq undo-tree-visualizer-relative-timestamps t)
      (setq undo-tree-visualizer-timestamps t)))
    (try-require 'undo-tree))
