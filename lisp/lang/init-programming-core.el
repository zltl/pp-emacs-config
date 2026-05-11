;;; init-programming-core.el --- Core programming tools -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module provides core programming infrastructure:
;; - Tree-sitter: Modern syntax parsing
;; - Eglot: Language Server Protocol support (built-in)
;; - Combobulate: Structural editing
;; - Common development tools
;;
;;; Code:

;; Hideshow - Built-in code folding
;; `hideshow' gives low-overhead folding for most programming modes, so
;; large functions can be collapsed without relying on LSP or tree-sitter.
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-c @ C-c" . hs-toggle-hiding)
              ("C-c @ C-a" . hs-hide-all)
              ("C-c @ C-s" . hs-show-all)
              ("C-c @ C-l" . hs-hide-level)))

;;; Tree-sitter Configuration
;;
;; Emacs 29+ has built-in tree-sitter support via `treesit`.
;; We no longer need the external tree-sitter/tree-sitter-langs packages
;; which require tsc-dyn.so (has GLIBC compatibility issues).

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  ;; Installing grammars on demand keeps first startup lighter while still
  ;; making one command enough to enable modern parsing/highlighting later.
  (defun ltl/setup-install-grammars ()
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
      ;; Register grammar sources centrally so installation and upgrades use
      ;; known-good repositories/versions compatible with this config.
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (progn
          (message "installing grammar: %s" grammar)
          (treesit-install-language-grammar (car grammar))))))

  ;; Remap classic major modes to their tree-sitter variants so users get
  ;; better syntax parsing automatically when grammars are available.
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
  ;; Register grammar sources but only install on demand
  (ltl/setup-install-grammars))

;;; Combobulate - Structural Editing

;; `combobulate' builds on tree-sitter to provide syntax-aware structural
;; editing, which is safer than plain text motions for nested languages.
(use-package combobulate
  :after treesit
  :ensure (combobulate :host github :repo "mickeynp/combobulate")
  :custom
  ;; Keep structural-editing commands under a dedicated prefix so they do
  ;; not clash with major-mode bindings.
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))

;;; Eglot Configuration

(defun ltl/eglot-ensure ()
  "Start Eglot in non-Emacs Lisp buffers."
  ;; Emacs Lisp has excellent native tooling already, so skipping Eglot
  ;; there avoids needless language-server startup and duplicate features.
  (unless (derived-mode-p 'emacs-lisp-mode)
    (eglot-ensure)))

;; `eglot' is the built-in LSP client, chosen here to keep the stack lean
;; while still providing rename, formatting, diagnostics, and navigation.
(use-package eglot
  :ensure nil
  :hook ((prog-mode . ltl/eglot-ensure))
  :bind (:map ltl/lsp-map
              ("r" . eglot-rename)
              ("f" . eglot-format)
              ("a" . eglot-code-actions)
              ("d" . eldoc-doc-buffer))
  :custom
  ;; Shut down idle language servers when their last managed buffer closes
  ;; so background processes do not accumulate across projects.
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0) ;; Disable logging for performance
  :config
  ;; Explicit TSX registration ensures tsx-ts-mode gets the correct server
  ;; even when default mode/server associations vary by package version.
  (add-to-list 'eglot-server-programs
               '(tsx-ts-mode . ("typescript-language-server" "--stdio"))))

;; Consult integration for Eglot
;; Search workspace symbols through Consult's preview/narrowing UI instead
;; of a plain completing-read prompt.
(use-package consult-eglot
  :after (consult eglot)
  :bind (:map ltl/lsp-map
              ("s" . consult-eglot-symbols)))

;;; Development Tools

;; `eldoc-box' shows hover docs in a child frame, making LSP help easier
;; to read than cramped echo-area messages.
(use-package eldoc-box
  :diminish
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

;; `compile-multi' stores multiple named build/test commands per project,
;; which is convenient when one repo has several entry points.
(use-package compile-multi
  :defer t
  :commands compile-multi)

;; `apheleia' offers asynchronous formatting, so buffer formatting does
;; not block the UI or require mode-specific formatter glue everywhere.
(use-package apheleia
  :defer t
  :commands (apheleia-mode apheleia-format-buffer))

;; Keep clang-format available as an explicit formatter for C/C++ buffers
;; and one-off region formatting.
(use-package clang-format
  :defer t
  :commands (clang-format-buffer clang-format-region))

;; `dumb-jump' is a practical fallback for jump-to-definition when no
;; language server is available or indexed yet.
(use-package dumb-jump
  :defer t
  :commands (dumb-jump-go dumb-jump-back))

;; `hl-todo' highlights actionable keywords inline so TODO/FIX/BUG notes
;; stay visible during normal editing and review.
(use-package hl-todo
  :ensure (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  ;; Extend the default keyword palette with this config's preferred task
  ;; vocabulary so local conventions are highlighted consistently.
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

;; `rainbow-mode' previews colors using their actual value, which is very
;; helpful when editing CSS, HTML, or design-token files.
(use-package rainbow-mode
  :ensure (rainbow-mode :host github :repo "emacsmirror/rainbow-mode" :branch "master")
  :defer t
  :hook ((css-mode scss-mode html-mode) . rainbow-mode))

;; `dap-mode' is kept on demand because debugging is important but not a
;; startup-critical workflow.
(use-package dap-mode
  :defer t
  :commands (dap-debug dap-debug-edit-template))

;;; Lisp Editing

;; `paredit' keeps Lisp structure balanced while editing, which is worth
;; the stricter behavior in Lispy modes.
(use-package paredit
  :diminish
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

;; Color each nesting depth differently so deeply nested forms are easier
;; to parse visually.
(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight the symbol at point and its siblings to speed up code reading
;; without a full semantic highlighter.
(use-package highlight-thing
  :diminish
  :hook (prog-mode . highlight-thing-mode))

;; `sly' is the Common Lisp IDE entry point, kept lazy because it depends
;; on an external Lisp runtime and is not always needed.
(use-package sly
  :defer t
  :commands (sly sly-connect))

;;; Project Management

;; `perspective' gives named workspaces, which helps keep unrelated
;; project/task window sets separate in long Emacs sessions.
(use-package perspective
  :defer t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              ;; Optional loading keeps the project-management layer from
              ;; breaking startup on machines that do not use Perspective.
              (when (require 'perspective nil t)
                (persp-mode 1)))))

(provide 'init-programming-core)
;;; init-programming-core.el ends here
