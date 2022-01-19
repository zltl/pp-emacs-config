;;; init-lsp.el --- config lsp
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (eval-after-load 'whichkey
    (progn
    (setq lsp-enable-file-watchers t)
    (lsp-enable-which-key-integration t))))

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
  (define-key lsp-ui-mode-map [remap xref-find-definitions]
    #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]
    #'lsp-ui-peek-find-references))

(setq lsp-headerline-arrow "/")

(provide 'init-lsp)
;;; init-lsp.el ends here
