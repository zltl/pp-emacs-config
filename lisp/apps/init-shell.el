;;; init-shell.el --- Terminal and shell configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures terminal emulators and shell integration:
;; - eat: Modern terminal emulator
;; - eshell: Built-in Emacs shell with custom prompt
;;
;;; Code:

;; eat - Emulate A Terminal
(use-package eat
  :ensure (eat :host codeberg
               :repo "akib/emacs-eat"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       ("e" "e/*")))
  :hook (eshell-load . eat-eshell-mode))

;; Eshell configuration
(use-package eshell
  :ensure nil
  :defer t
  :custom
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  :config
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize (abbreviate-file-name (eshell/pwd)) 'face 'font-lock-keyword-face)
           (when (and (fboundp 'magit-get-current-branch) (magit-get-current-branch))
             (propertize (concat " " (magit-get-current-branch)) 'face 'font-lock-string-face))
           (propertize " λ " 'face 'font-lock-constant-face))))
  (setq eshell-prompt-regexp "^.* λ "))

(provide 'init-shell)
;;; init-shell.el ends here
