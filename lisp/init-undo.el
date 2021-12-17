;;; init-undo.el --- undo tree
;;; Commentary:
;;; Code:

(require 'init-cachedir)

(defvar my-undo-cache-dir (expand-file-name "undo" my-cache-dir)
  "Undotree cache location.")

(use-package undo-tree
  :diminish nil
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
	(cons (cons "." my-undo-cache-dir) ())))

(provide 'init-undo)
;;; init-undo.el ends here
