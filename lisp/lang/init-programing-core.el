;;; init-programing-core.el --- Core programming tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides core programming infrastructure:
;; - Tree-sitter: Modern syntax parsing
;; - LSP Mode: Language Server Protocol support
;; - Combobulate: Structural editing
;; - Common development tools
;;
;;; Code:

;;; Tree-sitter Configuration
;;
;; Emacs 29+ has built-in tree-sitter support via `treesit`.
;; We no longer need the external tree-sitter/tree-sitter-langs packages
;; which require tsc-dyn.so (has GLIBC compatibility issues).

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
               (c . ("https://github.com/tree-sitter/tree-sitter-c"))
               (c-sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
               (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
               (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
               (commonlisp . ("https://github.com/tree-sitter-grammars/tree-sitter-commonlisp"))
               (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
               (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (java . ("https://github.com/tree-sitter/tree-sitter-java"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.23.2"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
               (make . ("https://github.com/tree-sitter-grammars/tree-sitter-make"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (perl . ("https://github.com/ganezdragon/tree-sitter-perl"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (r . ("https://github.com/r-lib/tree-sitter-r"))
               (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (scala . ("https://github.com/tree-sitter/tree-sitter-scala"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
               (vue . ("https://github.com/tree-sitter-grammars/tree-sitter-vue"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (progn
          (message "installing grammar: %s" grammar)
          (treesit-install-language-grammar (car grammar))))))

  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  (or (file-directory-p (expand-file-name "tree-sitter" user-emacs-directory))
      (progn
        (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))))

;;; Combobulate - Structural Editing

(use-package combobulate
  :after treesit
  :ensure (combobulate :host github :repo "mickeynp/combobulate")
  :custom
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))

;;; LSP Mode Configuration

(setq lsp-use-plists t)

(use-package lsp-mode
  :ensure (lsp-mode :host github :repo "emacs-lsp/lsp-mode")
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-render-all nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-completion-show-detail nil)
  (lsp-completion-show-kind nil))

(with-eval-after-load 'lsp-mode
  (progn
    (add-to-list 'lsp--formatting-indent-alist '(tsx-ts-mode . typescript-indent-level))
    (add-hook 'before-save-hook
              (lambda () (when (eq 'tsx-ts-mode major-mode)
                           (lsp-format-buffer))))))

;; LSP UI
(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)

;; LSP integration with other tools
(use-package helm-lsp
  :defer t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-ivy
  :defer t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :defer t
  :commands lsp-treemacs-errors-list)

;;; Development Tools

(use-package eldoc-box
  :defer t
  :diminish)

(use-package compile-multi
  :defer t
  :commands compile-multi)

(use-package apheleia
  :defer t
  :commands (apheleia-mode apheleia-format-buffer))

(use-package clang-format
  :defer t
  :commands (clang-format-buffer clang-format-region))

(use-package dumb-jump
  :defer t
  :commands (dumb-jump-go dumb-jump-back))

(use-package hl-todo
  :ensure (:host github :repo "tarsius/hl-todo")
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

(use-package rainbow-mode
  :ensure (rainbow-mode :host github :repo "emacsmirror/rainbow-mode" :branch "master")
  :defer t
  :hook ((css-mode scss-mode html-mode) . rainbow-mode))

(use-package dap-mode
  :defer t
  :commands (dap-debug dap-debug-edit-template))

;;; Lisp Editing

(use-package paredit
  :diminish
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-thing
  :diminish
  :hook (prog-mode . highlight-thing-mode))

(use-package sly
  :defer t
  :commands (sly sly-connect))

;;; Project Management

(use-package perspective
  :bind
  :custom
  (persp-mode-prefix-key  (kbd "C-c M-p"))
  :init
  (persp-mode))

(provide 'init-programing-core)
;;; init-programing-core.el ends here
