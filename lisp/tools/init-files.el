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
;; This makes C-x C-f reuse any filename/path at point, which speeds up
;; following references from logs, docs, or shell output.
(global-set-key (kbd "C-x C-f") #'find-file-at-point)
;; Standard switch-to-buffer
;; Keep C-x b on the familiar buffer switch command; Consult remaps it
;; later, but preserving the base binding keeps intent obvious here.
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
;; Auto-reverting keeps buffers in sync with external file changes from
;; Git, formatters, generators, or other editor sessions.
(use-package autorevert
  :ensure nil
  :diminish
  :hook (on-first-buffer . global-auto-revert-mode)
  :custom
  ;; Revert not only real files but also generated buffers like Dired or
  ;; process-backed listings when their source changes.
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
  ;; Exclude generated state directories so the recent file list stays
  ;; focused on real work instead of package/cache artifacts.
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
;; Auto-refresh saves a manual revert whenever files are created or
;; deleted outside the current Dired buffer.
(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-auto-revert-buffer t))

;; project.el - Built-in project management (replaces projectile)
;; Using built-in project.el lowers package surface area while still
;; covering the workflows that matter most: file search, grep, shells,
;; and Git status.
(use-package project
  :ensure nil
  :bind-keymap ("C-x p" . project-prefix-map)
  :custom
  ;; Curate the project switch menu around the commands used most often
  ;; so project dispatch feels like a focused workspace menu.
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find dir")
     (project-eshell "Eshell" ?e)
     (magit-project-status "Magit" ?m))))

;; TRAMP - Remote file editing
;; Keep remote editing available, but tune it toward responsiveness so
;; occasional SSH edits do not feel painfully slow.
(use-package tramp
  :ensure nil
  :defer t
  :custom
  ;; SSH is the most common and predictable remote transport for this setup.
  (tramp-default-method "ssh")
  ;; A low verbosity level keeps *Messages* readable while still showing
  ;; enough context when a remote connection fails.
  (tramp-verbose 1)
  ;; Limit cache lifetime so remote path metadata does not become stale
  ;; for long-lived Emacs sessions.
  (remote-file-name-inhibit-cache 60)
  :config
  ;; Disable vc on remote files for performance
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))

(provide 'init-files)
;;; init-files.el ends here
