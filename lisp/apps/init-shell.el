;;; init-shell.el --- Terminal emulator and shell configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures terminal emulators and shells:
;; - Eat: Modern terminal emulator for Emacs
;; - Eshell: Emacs built-in shell
;; - Vterm: Fast terminal emulator (if available)
;;
;;; Code:

(use-package eat
  :ensure (eat :host github :repo "jamescherti/emacs-eat"
               :inherit nil
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el")))
  :hook (eshell-load-hook . eat-eshell-mode))


(provide 'init-shell)
;;; init-shell.el ends here
