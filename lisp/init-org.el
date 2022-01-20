;;; init-org.el --- config org-mode
;;; Commentary:
;;; Code:

(defun my/org-mode-setup ()
    "Setup orgmode."
    (org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1))

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-ellipsis " ▼")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-states-order-reversed t)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
        (file-expand-wildcards "~/TODO/*.org"))
  (setq org-todo-keywords
		'(
		  (sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
		  (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)"))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;; no extra indentation in the source blocks
(setq org-src-preserve-indentation t)

(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode)
  :config
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-date)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-done)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-drawer)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-formula)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-tag)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-todo))

(provide 'init-org)
;;; init-org.el ends here
