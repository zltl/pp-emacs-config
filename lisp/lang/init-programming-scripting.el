;;; init-programming-scripting.el --- Scripting languages -*- lexical-binding: t; -*-

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

(declare-function ltl/eglot-ensure "init-programming-core")

;;; Python

;; Use built-in python.el (provides python-mode)
;; Keeping Python on the built-in major mode lowers package count while
;; still integrating cleanly with Eglot and tree-sitter remapping.
(use-package python
  :ensure nil  ; built-in, no need to install
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook ((python-mode python-ts-mode) . ltl/eglot-ensure))

;; `python-docstring' helps keep docstrings formatted to convention while
;; writing them, which reduces style cleanup later.
(use-package python-docstring
  :defer t
  :hook ((python-mode python-ts-mode) . python-docstring-mode))

;; `pet' detects project-specific Python tooling and environments, which
;; helps formatters/linters/LSP align with the active virtualenv/poetry/uv setup.
(use-package pet
  :hook ((python-base-mode python-ts-mode) . pet-mode))

;;; Lua

;; `lua-mode' provides straightforward syntax and indentation support for
;; Lua scripts, with 2-space indent to match common community style.
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

(provide 'init-programming-scripting)
;;; init-programming-scripting.el ends here
