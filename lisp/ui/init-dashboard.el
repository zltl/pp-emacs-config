;;; init-dashboard.el --- Dashboard configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures the startup dashboard.
;;
;;; Code:

;; `dashboard' gives a friendly landing page with recent files and
;; projects, which reduces friction when opening Emacs "just to jump
;; back into something".
(use-package dashboard
  :if ltl/enable-dashboard
  :config
  ;; Register the startup hook so the dashboard replaces the default splash buffer.
  (dashboard-setup-startup-hook)
  ;; Personalize the title so startup feels intentional rather than stock.
  (setq dashboard-banner-logo-title (concat  "Yo! master " sys/user))
  ;; Reuse built-in project.el for project discovery to stay consistent
  ;; with the rest of the configuration.
  (setq dashboard-projects-backend 'project-el)
  ;; A short list is enough to resume work. Agenda items would load Org
  ;; during startup; open a .org file or `M-x org-agenda' instead.
  (setq dashboard-items '((recents   . 8)
                          (bookmarks . 5)
                          (projects  . 5)
                          (registers . 3)))
  (setq dashboard-set-init-info t
        dashboard-set-footer nil))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
