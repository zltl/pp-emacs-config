

;; lsp
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  :config (setq lsp-enable-imenu nil)
  :hook (lsp-completion-mode . my/lsp-mode-setup-completion)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
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
  :hook (go-mode . lsp)
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-ts-mode-hook #'lsp-go-install-save-hooks))
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
(use-package lsp-metals
  :custom
  ;; You might set metals server options via -J arguments. This might not always work, for instance when
  ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
  (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                            ;; formatting of multiline strings only. You might want to disable it so that
                            ;; emacs can use indentation provided by scala-mode.
                            "-J-Dmetals.allow-multiline-string-formatting=off"
                            ;; Enable unicode icons. But be warned that emacs might not render unicode
                            ;; correctly in all cases.
                            "-J-Dmetals.icons=unicode"))
  ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
  ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
  ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
  ;; which is mapped to `keyword' face.
  (lsp-metals-enable-semantic-highlighting t)
  :hook (scala-mode . lsp))

(use-package julia-mode)
(use-package lua-mode)
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
  :hook (rust-mode . prettify-symbols-mode)
  :hook (rust-mode . lsp))

;; TypeScript, JS, and JSX/TSX support.
(use-package web-mode
  :mode ("\\.ts\\'"
         "\\.js\\'"
         "\\.mjs\\'"
         "\\.tsx\\'"
         "\\.jsx\\'"
         )
  :hook (web-mode . lsp)
  :custom
   (web-mode-markup-indent-offset 2)
   (web-mode-css-indent-offset 2)
   (web-mode-code-indent-offset 2))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(use-package clang-format)

(use-package bazel)

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))
(use-package python-mode
  :hook (python-mode . lsp))
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

;; Automatically install and use tree-sitter major modes in Emacs
;; 29+. If the tree-sitter version can’t be used, fall back to the
;; original major mode.
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  :hook ((c++-ts-mode . lsp)
         (go-ts-mode . lsp)
         (tsx-ts-mode . lsp)))

;; limit clangd resources
(setq lsp-clients-clangd-args '("--j=4" "--background-index=false" "--log=error"))

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
