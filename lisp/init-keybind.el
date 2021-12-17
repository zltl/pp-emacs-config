;;; init-keybind.el --- define key bindings
;;; Commentary:
;;; Code:

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; recentf
(global-set-key (kbd "C-x C-b") 'recentf-open-files)

;; counsel
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)

(provide 'init-keybind)
;;; init-keybind.el ends here
