;;; init-programing-misc.el --- Miscellaneous languages and configs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for:
;; - JVM languages (Scala, Java)
;; - Functional languages (Haskell, Elixir)
;; - Other languages (Julia, C#, PowerShell)
;; - Configuration file formats (YAML, TOML, Docker, etc.)
;;
;;; Code:

;;; JVM Languages

(use-package scala-mode
  :defer t
  :mode "\\.s\\(cala\\|bt\\)\\'"
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :defer t
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;;; Functional Languages

(use-package haskell-mode
  :defer t
  :mode "\\.l?hs\\'"
  :hook ((haskell-mode haskell-literate-mode) . eglot-ensure))

(use-package elixir-mode
  :defer t
  :mode "\\.exs?\\'")

;;; Other Languages

(use-package julia-mode
  :defer t
  :mode "\\.jl\\'")

(use-package csharp-mode
  :defer t
  :mode ((rx ".cs" eos) . 'csharp-ts-mode)
  :hook (csharp-ts-mode . subword-mode))

(use-package powershell
  :defer t
  :mode ("\\.ps[dm]?1\\'" . powershell-mode))

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'")

;;; Markup and Data Formats

(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :defer t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package esxml
  :defer t
  :commands (esxml-to-xml sxml-to-xml))

;;; Build Systems

(use-package cmake-mode
  :defer t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package bazel
  :defer t
  :mode (("BUILD\\'" . bazel-mode)
         ("WORKSPACE\\'" . bazel-mode)
         ("\\.BUILD\\'" . bazel-mode)
         ("\\.bazel\\'" . bazel-mode)
         ("\\.bzl\\'" . bazel-mode)))

;;; Configuration Files

(use-package gitlab-ci-mode
  :defer t
  :mode "\\.gitlab-ci\\.yml\\'")

(use-package vimrc-mode
  :defer t
  :mode (("\\.vim\\(rc\\)?\\'" . vimrc-mode)
         ("\\.exrc\\'" . vimrc-mode)))

(use-package cuda-mode
  :defer t
  :mode "\\.cu\\'")

(use-package dotenv-mode
  :defer t
  :mode "\\.env\\..*\\'")

(use-package gitignore-templates
  :defer t
  :commands (gitignore-templates-insert
             gitignore-templates-new-file))

(use-package dockerfile-mode
  :defer t
  :mode "Dockerfile\\'")

(use-package jenkinsfile-mode
  :defer t
  :mode "Jenkinsfile\\'")

(use-package crontab-mode
  :defer t
  :mode "/crontab\\(\\.X*[[:alnum:]]+\\)?\\'")

(use-package nginx-mode
  :defer t)

;;; Specialized Modes

(use-package fb-mode
  :ensure (:host github :repo "rversteegen/fb-mode")
  :defer t
  :commands fb-mode
  :mode "\\.b\\(i\\|as\\)\\'")

(use-package franca-idl
  :ensure (:host github :repo "zeph1e/franca-idl.el")
  :defer t
  :mode "\\.fidl\\'")

(provide 'init-programing-misc)
;;; init-programing-misc.el ends here
