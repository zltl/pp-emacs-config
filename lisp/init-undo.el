;;; init-undo.el --- undo tree

;;; Code:

(use-package undo-tree
  :diminish nil
  :config
  (defun undo-tree-visualizer-update-linum (&rest args)
    (linum-update undo-tree-visualizer-parent-buffer))
  (advice-add 'undo-tree-visualize-undo :after #'undo-tree-visualizer-update-linum)
  (advice-add 'undo-tree-visualize-redo :after #'undo-tree-visualizer-update-linum)
  (advice-add 'undo-tree-visualize-undo-to-x :after #'undo-tree-visualizer-update-linum)
  (advice-add 'undo-tree-visualize-redo-to-x :after #'undo-tree-visualizer-update-linum)
  (advice-add 'undo-tree-visualizer-mouse-set :after #'undo-tree-visualizer-update-linum)
  (advice-add 'undo-tree-visualizer-set :after #'undo-tree-visualizer-update-linum)
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t))

(provide 'init-undo)
;;; init-undo.el ends here
