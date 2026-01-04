;;; init-files.el --- File handling and project management -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures file handling and project management:
;; - Smart file opening (find-file-at-point)
;; - Recent files tracking (recentf)
;; - Auto-save and backup strategies
;; - No-littering for clean directory structure
;; - Projectile for project management
;;
;;; Code:

;;;
;;; File Operations Keybindings
;;;

;; Use find-file-at-point for smarter file opening
(global-set-key (kbd "C-x C-f") #'find-file-at-point)
;; Standard switch-to-buffer
(global-set-key (kbd "C-x b") #'switch-to-buffer)

;;;
;;; Lock files
;;;

;; On single-user environments, as we tend to run Emacs these days,
;; those .#* files are more likely to confuse some other program as
;; they are to protect us from conflicting edits.
(setopt create-lockfiles nil)

;; Junk drawer
;;
;; These customizations don’t fit anywhere else.
;; Remove the training wheels #
(put 'narrow-to-region 'disabled nil)

;; Auto-revert
(use-package autorevert
  :ensure nil
  :diminish
  :hook (on-first-buffer . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t))

;; Recent files
;; This maintains a list of recent files, as we often find in other
;; applications. I wonder if it can or should be integrated with
;; MacOS' list of recent files?
;; C-c f r open recentf
(use-package recentf
  :ensure nil
  :after no-littering
  :hook (on-first-file-hook . recentf-mode)
  :config
  (add-to-list 'recentf-exclude
	       (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
	       (recentf-expand-file-name no-littering-etc-directory))
  :bind
  (:map ltl/files-map
        ("r" . recentf-open)))

;; Dired
;; Dired should refresh the listing on each revisit.
;; C-\ to goggle input method
(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-auto-revert-buffer t))

(use-package projectile
  :diminish
  :hook (after-init . projectile-mode)
  :custom
  (projectile-completion-system 'default))

(provide 'init-files)
;;; init-files.el ends here
