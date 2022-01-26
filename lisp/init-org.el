;;; init-org.el --- config org-mode
;;; Commentary:
;;; Code:

(defun chinese/post-init-org ()
  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents))))

(defun my/org-mode-setup ()
    "Setup orgmode."
    (org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1)
	(chinese/post-init-org))

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

(use-package org-pomodoro)

(provide 'init-org)
;;; init-org.el ends here
