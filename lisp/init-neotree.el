;;; init-neotree.el --- config neotree
;;; Commentary:
;;; Code:

(use-package neotree
  :config
  ;; f8 to view tree strucure of folder
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  (global-set-key [f8] 'neotree-project-dir)
  ;; switch with projectile
  (use-package projectile)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq-default neo-show-hidden-files t))

(provide 'init-neotree)
;;; init-neotree.el ends here
