;;; init-programing-scripting.el --- Scripting languages -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for scripting languages:
;; - Python
;; - Lua
;; - Shell scripts
;; - Perl
;; - Ruby
;;
;;; Code:

;;; Python

;; Use built-in python.el (provides python-mode)
(use-package python
  :ensure nil  ; built-in, no need to install
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . eglot-ensure))

(use-package python-docstring
  :defer t
  :hook ((python-mode python-ts-mode) . python-docstring-mode))

(use-package anaconda-mode
  :defer t
  :hook (python-mode . anaconda-mode))

(use-package pet
  :defer t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (add-hook 'python-ts-mode-hook 'pet-mode -10))

;;; Lua

(use-package lua-mode
  :defer t
  :mode "\\.lua\\'"
  :interpreter "lua"
  :custom
  (lua-indent-level 2))

;;; Shell

;; Bash mode is built-in, configured via treesit

;;; Perl

;; Perl support via tree-sitter

;;; Ruby

;; Ruby support via tree-sitter

(provide 'init-programing-scripting)
;;; init-programing-scripting.el ends here
