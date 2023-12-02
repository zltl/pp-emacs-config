;;; init-programing.el --- Programming stuff -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :hook (minemacs-after-startup . global-treesit-auto-mode)
  :hook (minemacs-build-functions . treesit-auto-install-all)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  (push (make-treesit-auto-recipe
         :lang 'nix
         :ts-mode 'nix-ts-mode
         :remap 'nix-mode
         :url "https://github.com/nix-community/tree-sitter-nix"
         :ext "\\.nix\\'")
        treesit-auto-recipe-list)
  (setq treesit-auto-langs (seq-map #'treesit-auto-recipe-lang treesit-auto-recipe-list)))

(use-package awk-ts-mode)

(use-package combobulate
  :straight t
  :hook ((python-ts-mode
          js-ts-mode
          css-ts-mode
          yaml-ts-mode
          typescript-ts-mode
          tsx-ts-mode) . combobulate-mode)
  :custom
  (combobulate-key-prefix "C-c o"))


(defun +eglot-register (modes &rest servers)
  "Register MODES with LSP SERVERS.
Examples:
  (+eglot-register 'vhdl-mode \"vhdl_ls\")
  (+eglot-register 'lua-mode \"lua-language-server\" \"lua-lsp\")
  (+eglot-register '(c-mode c++-mode) '(\"clangd\" \"--clang-tidy\" \"-j=12\") \"ccls\")"
  (declare (indent 0))
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     (cons modes (if (length> servers 1)
                     (eglot-alternatives (ensure-list servers))
                   (ensure-list (car servers)))))))

(use-package eglot
  :straight t
  :hook (eglot-managed-mode . eglot-inlay-hints-mode)
  :custom
  (eglot-autoshutdown t) ; shutdown after closing the last managed buffer
  (eglot-sync-connect 0) ; async, do not block
  (eglot-extend-to-xref t) ; can be interesting!
  (eglot-report-progress nil) ; disable annoying messages in echo area!
  :config
  (+eglot-register
    '(c++-mode c++-ts-mode c-mode c-ts-mode)
    '("clangd"
      "--background-index"
      "-j=12"
      "--query-driver=/usr/bin/**/clang-*,/bin/clang,/bin/clang++,/usr/bin/gcc,/usr/bin/g++"
      "--clang-tidy"
      ;; "--clang-tidy-checks=*"
      "--all-scopes-completion"
      "--cross-file-rename"
      "--completion-style=detailed"
      "--header-insertion-decorators"
      "--header-insertion=iwyu"
      "--pch-storage=memory")
    "ccls")

  (+eglot-register '(awk-mode awk-ts-mode) "awk-language-server"))

(use-package consult-eglot
  :after consult eglot)

(use-package eldoc-box
  :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(use-package compile-multi)

(use-package apheleia)

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))

(use-package clang-format)
(use-package gitlab-ci-mode)
(use-package vimrc-mode)
(use-package rust-mode)
(use-package cuda-mode)
(use-package opencl-mode)
(use-package dumb-jump)
(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        (append
         hl-todo-keyword-faces
         '(("BUG"   . "#ee5555")
           ("FIX"   . "#0fa050")
           ("PROJ"  . "#447f44")
           ("IDEA"  . "#0fa050")
           ("INFO"  . "#0e9030")
           ("TWEAK" . "#fe9030")
           ("PERF"  . "#e09030")))))
(use-package rainbow-mode)
(use-package lua-mode
  :custom
  (lua-indent-level 2))
(use-package fb-mode
  :straight (:host github :repo "rversteegen/fb-mode")
  :commands fb-mode
  :mode "\\.b\\(i\\|as\\)\\'")
(use-package franca-idl
  :straight (:host github :repo "zeph1e/franca-idl.el"))
(use-package makefile-executor
  :straight t
  :hook (makefile-mode . makefile-executor-mode))
(use-package python-docstring
  :hook ((python-mode python-ts-mode) . python-docstring-mode))

(use-package dap-mode)

;; As you've probably noticed, Lisp has a lot of parentheses.
;; Maintaining the syntactical correctness of these parentheses
;; can be a pain when you're first getting started with Lisp,
;; especially when you're fighting the urge to break up groups
;; of closing parens into separate lines. Luckily we have
;; Paredit, a package that maintains the structure of your
;; parentheses for you. At first, Paredit might feel a little
;; odd; you'll probably need to look at a tutorial (linked
;; below) or read the docs before you can use it effectively.
;; But once you pass that initial barrier you'll write Lisp
;; code like it's second nature.
;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
;; https://stackoverflow.com/a/5243421/3606440
(use-package paredit
  :diminish
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

;; color parens
(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-thing
  :diminish
  :hook (prog-mode . highlight-thing-mode))

(use-package sly)

;; Nix
(use-package nix-mode
  :defer t)

;; XML
;; esxml essentially turns Lisp into an XML (or XHTML) templating
;; engine.
(use-package esxml
  :defer t)

;; YAML
(use-package yaml-mode
  :defer t)

;; C#
;; I am not a C# developer, but I’ve been known to interview them.
(use-package csharp-mode
  :mode ((rx ".cs" eos) . 'csharp-ts-mode)
  :hook (csharp-ts-mode . subword-mode))
(use-package elixir-mode)
;; NOTE: too slow pulling
(use-package cmake-mode)
(use-package go-mode
  :hook (go-mode . eglot)
  :hook (go-ts-mode .eglot))
(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))
;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package julia-mode)
(use-package markdown-mode
  :ensure t
  ;; These extra modes help clean up the Markdown editing experience.
  ;; `visual-line-mode' turns on word wrap and helps editing commands
  ;; work with paragraphs of text. `flyspell-mode' turns on an
  ;; automatic spell checker.
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package dockerfile-mode)

;; Note that `php-mode' assumes php code is separate from HTML.
;; If you prefer working with PHP and HTML in a single file you
;; may prefer `web-mode'.
(use-package php-mode)

;; powershell mode
(use-package powershell)

(use-package rust-mode
  :bind (:map rust-mode-map
	      ("C-c C-r" . 'rust-run)
	      ("C-c C-c" . 'rust-compile)
	      ("C-c C-f" . 'rust-format-buffer)
	      ("C-c C-t" . 'rust-test))
  :hook (rust-mode . prettify-symbols-mode))

;; TypeScript, JS, and JSX/TSX support.
(use-package web-mode
  :mode ("\\.ts\\'"
         "\\.js\\'"
         "\\.mjs\\'"
         "\\.tsx\\'"
         "\\.jsx\\'"
         )
  :custom
   (web-mode-markup-indent-offset 2)
   (web-mode-css-indent-offset 2)
   (web-mode-code-indent-offset 2))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(use-package bazel)

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))
(use-package python-mode
  ;; :hook (python-mode . lsp)
  )
(use-package anaconda-mode
  :hook (python-mode . anaconda-mode))

;; workspaces
(use-package perspective
  :bind
  ;; ("C-x C-b" . perp-list-buffers)
  :custom
  (persp-mode-prefix-key  (kbd "C-c M-p"))
  :init
  (persp-mode))

;; htmlize
;; htmlize provides syntax highlighting for our code snippets when
;; exported to HTML.
(use-package htmlize
  :after ox-html)

;; rime
;; install librime/librime-dev
(use-package rime
  :custom
  (default-input-method "rime"))

(provide 'init-programing)
