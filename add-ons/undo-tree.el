(defcustom addon-undo-tree
  nil
  "Enable undo-tree features"
  :type 'boolean
  :group 'features)

(when addon-undo-tree
  (progn
    (add-to-list 'my-default-packages 'undo-tree)
    (try-require 'undo-tree)))

(after 'undo-tree
  (global-undo-tree-mode)
  (setq undo-tree-outer-limit 36000000)
  (setq undo-tree-mode-lighter "")
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))
