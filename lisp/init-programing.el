;;; init-programing.el --- Programming stuff -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; indent 2 space on web
(setq js-indent-level 2)
(setq js-jsx-indent-level 2)
(setq typescript-indent-level 2)
(setq typescript-ts-mode-indent-offset 2)
(setq typescript-auto-indent-flag t
      typescript-ts-mode-indent-offset 2)

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  ;; TODO: M-x treesit-auto-install-all
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  (delete 'go treesit-auto-langs)
  ;; (delete 'tsx treesit-auto-langs)
  ;; (delete 'jsx treesit-auto-langs)
  )
(use-package cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))
(use-package protobuf-mode)
(use-package awk-ts-mode)

(setq lsp-use-plists t)
(use-package lsp-mode
  :commands (lsp lsp-deffered)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp)
         (go-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (python-mode . lsp)
         (python-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         (js-mode . lsp)
         (web-mode . lsp-deferred)
         (c-mode . lsp)
         (c++-mode . lsp)
         (c++-ts-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (typescriptreact-mode . lsp))
  :custom
  (lsp-completion-provider :none)
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-clients-clangd-args '("--background-index=false"
                             "--query-driver=/usr/bin/**/clang-*,/bin/clang,/bin/clang++,/usr/bin/gcc,/usr/bin/g++"
                             "--clang-tidy"
                             ;; "--clang-tidy-checks=*"
                             "--all-scopes-completion"
                             "--cross-file-rename"
                             "--completion-style=detailed"
                             "--header-insertion-decorators"
                             "--header-insertion=iwyu"
                             "--pch-storage=memory"))
  :config
  (defun corfu-lsp-setup ()
    (setq-local completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup))

(with-eval-after-load 'lsp-mode
  (progn
    (add-to-list 'lsp--formatting-indent-alist '(tsx-ts-mode . typescript-indent-level))
    (add-hook 'before-save-hook
              (lambda () (when (eq 'tsx-ts-mode major-mode)
                           (lsp-format-buffer))))))

(use-package tide
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :indentSize 2 :tabSize 2)))

(with-eval-after-load 'go-mode
  (progn
    (add-hook 'go-mode-hook
              #'(lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))
    (add-hook 'go-mode-hook
              #'(lambda () (add-hook 'before-save-hook #'lsp-organize-imports nil 'local)))))

(with-eval-after-load 'go-ts-mode
  (progn
    (add-hook 'go-ts-mode-hook
              #'(lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))
    (add-hook 'go-ts-mode-hook
              #'(lambda () (add-hook 'before-save-hook #'lsp-organize-imports nil 'local)))))


;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package eldoc-box
  :diminish)

(use-package compile-multi)

(use-package apheleia)

(use-package clang-format)
(use-package gitlab-ci-mode)
(use-package vimrc-mode)
(use-package rust-mode)
(use-package cuda-mode)
;; (use-package opencl-mode)
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
(use-package go-mode)
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
  :mode ("README\\.md\\'" . gfm-mode)
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
;;(use-package php-mode)

;; powershell mode
(use-package powershell)

(use-package rust-mode
  :bind (:map rust-mode-map
	      ("C-c C-r" . 'rust-run)
	      ("C-c C-c" . 'rust-compile)
	      ("C-c C-f" . 'rust-format-buffer)
	      ("C-c C-t" . 'rust-test))
  :hook (rust-mode . prettify-symbols-mode))

(use-package haskell-mode)
(use-package lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
;; (use-package apheleia
;;   :ensure t
;;   :config
;;   (apheleia-global-mode +1))


(use-package bazel)

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))
(use-package python-mode)
(use-package anaconda-mode
  :hook (python-mode . anaconda-mode))
(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (add-hook 'python-ts-mode-hook 'pet-mode -10))

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
;; (use-package htmlize
;;   :after ox-html)

;; .env
(use-package dotenv-mode
  :mode "\\.env\\..*\\'")
;; .gitignore
(use-package gitignore-templates
  :commands (gitignore-templates-insert
	     gitignore-templates-new-file))
;; docker
(use-package dockerfile-mode
  :mode "Dockerfile\\'")
;; jenkins
(use-package jenkinsfile-mode
  :mode "Jenkinsfile\\'")
;; crontab
(use-package crontab-mode
  :mode "/crontab\\(\\.X*[[:alnum:]]+\\)?\\'")
(use-package nginx-mode)

(provide 'init-programing)
;;; init-programing.el ends here
