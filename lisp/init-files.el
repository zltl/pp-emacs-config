;;; init-files.el --- config abount files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Lock files
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
  :hook (on-first-file-hook . recentf-mode)
  :bind
  (:map ltl/files-map
        ("r" . recentf-open)))

(use-package projectile
  :diminish
  :config
  (projectile-mode))

;; ffap, short for “find file at point,” guesses a default file from
;; the point. ffap-bindings rebinds several commands with ffap
;; equivalents.
(use-package ffap
  :hook (on-first-input . ffap-bindings))

;; Counting words
;; The default binding of M-= is count-words-region. The newer
;; count-words counts the buffer when there’s no active region.
(bind-key [remap count-words-region] 'count-words)

;; Dired
;; Dired should refresh the listing on each revisit.
;; C-\ to goggle input method
(require 'dired)
(setf dired-auto-revert-buffer t)

(provide 'init-files)
;;; init-files.el ends here
