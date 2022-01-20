;;; init-recentf.el --- setting of recentf
;;; Commentary:
;;; Code:

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

(use-package recentf
  :config
  (recentf-mode 1)
  (global-set-key (kbd "C-x C-b") #'recentf-open-files)
  (setq recentf-max-saved-items 10000)
  (setq recentf-max-menu-items 5000)
  (setq recentf-auto-cleanup 'never)
  (add-hook 'focus-out-hook #'my/recentf-save-list-silence t nil)
  (add-hook 'focus-out-hook #'my/recentf-cleanup-silence t nil)
  (with-eval-after-load 'no-littering
	(add-to-list 'recentf-exclude no-littering-etc-directory)
	(add-to-list 'recentf-exclude no-littering-var-directory))
  (add-to-list 'recentf-exclude (expand-file-name "elpa" user-emacs-directory))
  (add-to-list 'recentf-exclude (expand-file-name "cache" user-emacs-directory)))
   
(provide 'init-recentf)
;;; init-recentf.el ends here
