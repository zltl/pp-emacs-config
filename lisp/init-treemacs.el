;;; init-treemacs.el -- treemacs config
;;; commentary:
;;; Code:

(use-package treemacs
  :commands (treemacs
             treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode)
  :bind (("<f7>" . treemacs)
         ("<f6>" . treemacs-select-window))
  :init
  (setq treemacs-width 40
        ;; treemacs-is-never-other-window t
        ;; treemacs-indentation 0
        ;; treemacs-space-between-root-nodes nil
		)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode 1)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode nil)
  (other-window 1))

(use-package treemacs-projectile
  :after (treemacs projectile))
(use-package treemacs-magit
  :after (treemacs magit))

(provide 'init-treemacs)
;;; init-treemacs.el ends here

