;;; init-undo.el --- undo tree
;;; Commentary:
;;; Code:

(use-package undo-tree
  :diminish nil
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t))

(provide 'init-undo)
;;; init-undo.el ends here
