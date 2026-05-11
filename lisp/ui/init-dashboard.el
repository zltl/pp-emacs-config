;;; init-dashboard.el --- Dashboard configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures the startup dashboard.
;;
;;; Code:

;; `dashboard' gives a friendly landing page with recent files, projects,
;; and agenda items, which reduces friction when opening Emacs "just to
;; jump back into something".
(use-package dashboard
  :config
  ;; Register the startup hook so the dashboard replaces the default splash buffer.
  (dashboard-setup-startup-hook)
  ;; Personalize the title so startup feels intentional rather than stock.
  (setq dashboard-banner-logo-title (concat  "Yo! master " sys/user))
  ;; Reuse built-in project.el for project discovery to stay consistent
  ;; with the rest of the configuration.
  (setq dashboard-projects-backend 'project-el)
  ;; Favor recent files and projects because those are the fastest ways to
  ;; resume work after startup.
  (setq dashboard-items '((recents   . 50)
                        (bookmarks . 5)
                        (projects  . 5)
                        (agenda    . 5)
                        (registers . 5))))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
