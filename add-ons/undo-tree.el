(defcustom addon-undo-tree
  nil
  "Enable undo-tree features"
  :type 'boolean
  :group 'features)

(when addon-undo-tree
  (progn
    (add-to-list 'my-default-packages 'undo-tree)
    (with-library 'undo-tree
      (after 'undo-tree
        (global-undo-tree-mode t)
        (setq undo-tree-mode-lighter "")
        (setq undo-tree-visualizer-relative-timestamps t)
        (setq undo-tree-visualizer-timestamps t)))))
