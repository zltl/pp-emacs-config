;;; init-files.el --- File handling and project management -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures file handling and project management:
;; - Smart file opening (find-file-at-point)
;; - Recent files tracking (recentf)
;; - Auto-save and backup strategies
;; - No-littering for clean directory structure
;; - project.el for project management (built-in)
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

;; project.el - Built-in project management (replaces projectile)
(use-package project
  :ensure nil
  :bind-keymap ("C-x p" . project-prefix-map)
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find dir")
     (project-eshell "Eshell" ?e)
     (magit-project-status "Magit" ?m))))

;; TRAMP - Remote file editing
(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-default-method "ssh")
  (tramp-verbose 1)
  (remote-file-name-inhibit-cache 60)
  :config
  ;; Disable vc on remote files for performance
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))

(provide 'init-files)
;;; init-files.el ends here
