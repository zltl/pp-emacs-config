


(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title (concat  "Yo! master " sys/user))
  (setq dashboard-items '((recents   . 50)
                        (bookmarks . 5)
                        (projects  . 5)
                        (agenda    . 5)
                        (registers . 5))))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
