;;; init-lsp.el --- config lsp
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  ((c-mode; ccls
	c++-mode; ccls
	c-or-c++-mode ; ccls
	js-jsx-mode ; ts-ls
	typescript-mode ; ts-ls
	js-mode ; ts-ls
	python-mode; pyright
	web-mode; ts-ls/HTML/CSS
	haskell-mode; haskell-language-server
	go-mode
	) . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (with-eval-after-load 'whichkey
    (setq lsp-enable-file-watchers t)
    (lsp-enable-which-key-integration t)))

(use-package ccls)

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))


;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :requires lsp-mode flycheck
  :after (lsp-mode)
  :commands (lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ;; use lsp to search definitions (M-.)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ;; use lsp to search references (M-?)
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ;; symbol list
        ("C-c u" . lsp-ui-imenu))
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05)
  (define-key lsp-ui-mode-map [remap xref-find-definitions]
    #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]
    #'lsp-ui-peek-find-references))

(setq lsp-headerline-arrow "/")

(provide 'init-lsp)
;;; init-lsp.el ends here
