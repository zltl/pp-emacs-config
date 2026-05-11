;;; init-programming-misc.el --- Miscellaneous languages and configs -*- lexical-binding: t; -*-

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

;; `scala-mode' handles both Scala sources and SBT build files, which
;; covers the common JVM editing entry points without extra setup.
(use-package scala-mode
  :defer t
  :mode "\\.s\\(cala\\|bt\\)\\'"
  :interpreter ("scala" . scala-mode))

;; `sbt-mode' gives an integrated entry point to the Scala build tool and
;; disables supershell to keep command output compatible with Emacs buffers.
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

;; `haskell-mode' provides editing support, while Eglot supplies the IDE
;; layer when a language server is available.
(use-package haskell-mode
  :defer t
  :mode "\\.l?hs\\'"
  :hook ((haskell-mode haskell-literate-mode) . eglot-ensure))

;; `elixir-mode' is enough to get syntax support for Elixir scripts and modules.
(use-package elixir-mode
  :defer t
  :mode "\\.exs?\\'")

;;; Other Languages

;; `julia-mode' adds syntax and indentation support for Julia notebooks/scripts.
(use-package julia-mode
  :defer t
  :mode "\\.jl\\'")

;; Prefer the newer tree-sitter C# mode while still using csharp-mode's
;; file association package; subword helps camelCase navigation.
(use-package csharp-mode
  :defer t
  :mode ((rx ".cs" eos) . 'csharp-ts-mode)
  :hook (csharp-ts-mode . subword-mode))

;; `powershell' is useful for editing Windows automation scripts from any platform.
(use-package powershell
  :defer t
  :mode ("\\.ps[dm]?1\\'" . powershell-mode))

;; `nix-mode' gives syntax support for declarative Nix expressions and configs.
(use-package nix-mode
  :defer t
  :mode "\\.nix\\'")

;;; Markup and Data Formats

;; `markdown-mode' covers both generic Markdown and GitHub-flavored README
;; workflows, with visual line wrapping and spell check for prose quality.
(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode))
  :init
  ;; Multimarkdown offers solid export behavior and broad syntax support.
  (setq markdown-command "multimarkdown"))

;; `yaml-mode' provides indentation and syntax support for YAML config files.
(use-package yaml-mode
  :defer t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; `esxml' is a utility library for generating XML/SXML programmatically;
;; keeping it available helps when Emacs Lisp needs to emit markup.
(use-package esxml
  :defer t
  :commands (esxml-to-xml sxml-to-xml))

;;; Build Systems

;; `cmake-mode' gives a decent editing experience for CMake build files.
(use-package cmake-mode
  :defer t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; `bazel' handles BUILD/WORKSPACE/BZL files that are otherwise plain text.
(use-package bazel
  :defer t
  :mode (("BUILD\\'" . bazel-mode)
         ("WORKSPACE\\'" . bazel-mode)
         ("\\.BUILD\\'" . bazel-mode)
         ("\\.bazel\\'" . bazel-mode)
         ("\\.bzl\\'" . bazel-mode)))

;;; Configuration Files

;; `gitlab-ci-mode' adds syntax awareness to GitLab CI pipelines, helping
;; avoid indentation/key mistakes in YAML-heavy CI configs.
(use-package gitlab-ci-mode
  :defer t
  :mode "\\.gitlab-ci\\.yml\\'")

;; `vimrc-mode' is helpful when this Emacs config also edits legacy Vim config files.
(use-package vimrc-mode
  :defer t
  :mode (("\\.vim\\(rc\\)?\\'" . vimrc-mode)
         ("\\.exrc\\'" . vimrc-mode)))

;; `cuda-mode' improves readability of CUDA kernels and mixed C/C++ GPU code.
(use-package cuda-mode
  :defer t
  :mode "\\.cu\\'")

;; `dotenv-mode' gives light structure to .env files so secrets/config
;; files are not edited as undifferentiated plain text.
(use-package dotenv-mode
  :defer t
  :mode "\\.env\\..*\\'")

;; `gitignore-templates' speeds up creating ignore files from known templates.
(use-package gitignore-templates
  :defer t
  :commands (gitignore-templates-insert
             gitignore-templates-new-file))

;; `dockerfile-mode' handles Docker build files with proper syntax support.
(use-package dockerfile-mode
  :defer t
  :mode "Dockerfile\\'")

;; `jenkinsfile-mode' avoids editing Jenkins pipelines as plain Groovy text.
(use-package jenkinsfile-mode
  :defer t
  :mode "Jenkinsfile\\'")

;; `crontab-mode' is a tiny but useful quality-of-life mode for cron schedules.
(use-package crontab-mode
  :defer t
  :mode "/crontab\\(\\.X*[[:alnum:]]+\\)?\\'")

;; `nginx-mode' provides syntax support for Nginx server configs.
(use-package nginx-mode
  :defer t)

;;; Data Formats

;; `csv-mode' turns spreadsheets-in-text into something editable: aligned
;; columns and CSV-aware movement reduce mistakes in tabular data files.
(use-package csv-mode
  :defer t
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :hook (csv-mode . csv-align-mode)
  :custom
  ;; Add breathing room between columns so aligned CSV is easier to scan.
  (csv-align-padding 2))

;;; Specialized Modes

;; `fb-mode' supports FreeBASIC files, which are uncommon enough to keep lazy.
(use-package fb-mode
  :ensure (:host github :repo "rversteegen/fb-mode")
  :defer t
  :commands fb-mode
  :mode "\\.b\\(i\\|as\\)\\'")

;; `franca-idl' covers FIDL files used in automotive/service-interface workflows.
(use-package franca-idl
  :ensure (:host github :repo "zeph1e/franca-idl.el")
  :defer t
  :mode "\\.fidl\\'")

(provide 'init-programming-misc)
;;; init-programming-misc.el ends here
