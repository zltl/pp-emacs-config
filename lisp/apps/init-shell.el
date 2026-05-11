;;; init-shell.el --- Terminal and shell configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures terminal emulators and shell integration:
;; - eat: Modern terminal emulator
;; - eshell: Built-in Emacs shell with custom prompt
;;
;;; Code:

;; eat - Emulate A Terminal
;; `eat' is the preferred terminal backend here because it handles modern
;; terminal features better than older Emacs terminal emulators.
(use-package eat
  :ensure (eat :host codeberg
               :repo "akib/emacs-eat"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       ("e" "e/*")))
  ;; Reuse Eat inside Eshell so shell commands with terminal control codes
  ;; render correctly instead of degrading to plain text output.
  :hook (eshell-load . eat-eshell-mode))

;; Eshell configuration
;; `eshell' stays useful as a Lisp-native shell for quick project-local
;; commands, especially when you want buffer integration over a full TTY.
(use-package eshell
  :ensure nil
  :defer t
  :custom
  ;; Keep the prompt/input visible during long command output.
  (eshell-scroll-to-bottom-on-input 'all)
  ;; Auto-clean finished process buffers so temporary shell commands do
  ;; not leave dead buffers lying around.
  (eshell-destroy-buffer-when-process-dies t)
  ;; Skip duplicate history entries to make shell recall more meaningful.
  (eshell-hist-ignoredups t)
  ;; Persist history between sessions so project shell commands are easy to revisit.
  (eshell-save-history-on-exit t)
  :config
  ;; Show cwd and current Git branch in the prompt so project context is
  ;; obvious even when several shell buffers are open.
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize (abbreviate-file-name (eshell/pwd)) 'face 'font-lock-keyword-face)
           (when (and (fboundp 'magit-get-current-branch) (magit-get-current-branch))
             (propertize (concat " " (magit-get-current-branch)) 'face 'font-lock-string-face))
           (propertize " λ " 'face 'font-lock-constant-face))))
  ;; Match the custom prompt precisely so Eshell can recognize prompt boundaries.
  (setq eshell-prompt-regexp "^.* λ "))

(provide 'init-shell)
;;; init-shell.el ends here
