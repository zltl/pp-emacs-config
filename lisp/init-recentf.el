;;; init-recentf.el --- setting of recentf

;;; Code:

(require 'recentf)

(require 'init-cachedir)
(setq recentf-save-file (expand-file-name "recentf" my-cache-dir))

(defun my/recentf-save-list-silence ()
  "Save recentf."
  (interactive)
  (let ((mesage-log-max nil))
    (if (fboundp 'shut-up)
        (shut-up (recentf-save-list))
      (recentf-save-list)))
  (message ""))

(defun my/recentf-cleanup-silence()
  "Clean recentf."
  (interactive)
  (let ((message-log-max nil))
    (if (fboundp 'shut-up)
        (shut-up (recentf-cleanup))
      (recentf-cleanup)))
  (message ""))

(setq recentf-max-saved-items 10000)
(setq recentf-max-menu-items 5000)
(setq recentf-auto-cleanup 'never)
(add-hook 'focus-out-hook #'my/recentf-save-list-silence t nil)
(add-hook 'focus-out-hook #'my/recentf-cleanup-silence t nil)

(recentf-mode 1)

(provide 'init-recentf)
;;; init-recentf.el ends here
