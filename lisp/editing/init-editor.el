;;; init-editor.el --- Editor behavior and editing enhancements -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures core editing behaviors including:
;; - State persistence (save-place, recentf)
;; - Whitespace handling and cleanup
;; - Auto-pairing (parentheses, quotes)
;; - Selection and deletion behaviors
;; - Undo/redo functionality
;; - Line numbers and visual aids
;; - And more editing conveniences
;;
;;; Code:

;; Persist state
;; Persist State flushes state that is normally flushed in
;; kill-emacs-hook, which I'm trying not to call until I die.
(use-package persist-state
  :ensure (persist-state :host github :repo "emacsmirror/persist-state" :inherit nil)
  :diminish
  :config
  (persist-state-mode))

;; Newline at end of file
(setq require-final-newline t)

;; This mode saves our place for when we revisit a file.
(use-package saveplace
  :ensure nil
  :hook (on-first-buffer . save-place-mode)
  :config
  (setq save-place-file (expand-file-name "var/.saved-places" user-emacs-directory)))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(use-package super-save
  :defer 1
  :diminish
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; Subword mode helps us move around camel-case languages, and is
;; mostly configured as a hook in those major modes. The only thing we
;; customize about it is not wanting it cluttering the mode line.
(use-package subword
  :ensure nil
  :diminish)

;;; Exiting
;; I’d usually rather exit Slack, to be quite honest.
(setopt confirm-kill-emacs 'yes-or-no-p)

;; Add unique buffer names in the minibuffer where there are many
;; identical files. This is super useful if you rely on folders for
;; organization and have lots of files with the same name,
;; e.g. foo/index.ts and bar/index.ts.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      ;; Backups are placed into your Emacs directory, e.g. xxxx/backups
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      delete-old-versions 1
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Character radix
;; Make C-q read a hex sequence instead of the default octal. Obscure,
;; but I know more characters by their hex codes. This is also
;; consistent with C-x 8 <RET>, which is more chars, but offers
;; minibuffer completion.
(setopt read-quoted-char-radix 16)

;; Delete Selection Mode #
;; Typing over an active section should delete the section.
(use-package delsel
  :ensure nil
  :custom
  (delete-selection-mode))

;; set-mark-command-repeat-pop means we only need to hit C-u or C-x
;; once before subsequent C-SPC, which makes it much nicer to
;; navigate.
(setopt set-mark-command-repeat-pop t)

;; indent

;; Tabs are the devil’s whitespace.
;; Killing
;; Put the clipboard on the kill ring before killing something
;; else. Emacs isn’t as violent as it sometimes sounds, I swear.
;;
;; We also don’t want to clutter the ring with consecutively duplicate
;; values.
(setf save-interprogram-paste-before-kill t)
(setf kill-do-not-save-duplicates t)
(setq-default indent-tabs-mode nil)

;; editorconfig for emacs
(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(use-package smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	       ([remap delete-backward-char] . smart-hungry-delete-backward-char)
	       ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

;; ;; space between chinese and english
;; (use-package pangu-spacing
;;   :diminish
;;   :config
;;   ;; (global-pangu-spacing-mode 1)
;;   )
;; (add-hook 'org-mode-hook
;;           #'(lambda ()
;;               (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))
;; (add-hook 'markdown-mode-hook
;;           #'(lambda ()
;;               (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

;; Persist bookmarks each time we set one, not when Emacs exits.
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1))

;; Automatically insert closing parens
(electric-pair-mode t)
;; Enable electric indent mode for auto-indentation on RET
(electric-indent-mode t)
;; Visualize matching parens
(show-paren-mode 1)
;; Prefer spaces to tabs
(setq-default indent-tabs-mode nil)
;; Note: save-place-mode is configured above with use-package
;; Save history in minibuffer to keep recent commands easily accessible
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      savehist-file (expand-file-name
                     "history"
                     (ltl/ensure-dir
                      (expand-file-name "var" user-emacs-directory))))
(savehist-mode +1)

;; Keep files up-to-date when they change outside Emacs
(global-auto-revert-mode t)

(use-package smartparens
  :ensure t
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config)
  (defvar-keymap sp/forward-barf|slurp
    :doc "Forward barf/slurp"
    "b" #'sp-forward-barf-sexp
    "s" #'sp-forward-slurp-sexp)
  (defvar-keymap sp/backward-barf|slurp
    :doc "Backward barf/slurp"
    "b" #'sp-backward-barf-sexp
    "s" #'sp-backward-slurp-sexp)
  (defvar-keymap sp/navigation
    :doc "navigation"
    "f" #'sp-forward-sexp
    "b" #'sp-backward-sexp
    "n" #'sp-next-sexp
    "p" #'sp-previous-sexp
    "d" #'sp-down-sexp
    "a" #'sp-beginning-of-sexp
    "e" #'sp-end-of-sexp
    "u" #'sp-up-sexp)
  (keymap-set ltl/smartparen "f" sp/forward-barf|slurp)
  (keymap-set ltl/smartparen "b" sp/backward-barf|slurp)
  (keymap-set ltl/smartparen "n" sp/navigation)
  (which-key-add-keymap-based-replacements ltl/smartparen
    "f" `("Forward" . ,sp/forward-barf|slurp)
    "b" `("Backward" . ,sp/backward-barf|slurp)
    "n" `("Navigation" . ,sp/navigation))
  :bind
  (:map ltl/smartparen
        ("h" . #'sp-cheat-sheet)
        ("k" . #'sp-kill-sexp)
        ("u" . #'sp-unwrap-sexp)))

;; multi cursor
(use-package multiple-cursors
  :bind
  (:map ltl/multicursor
        ("l" . #'mc/edit-lines)
        ("a" . #'mc/edit-beginnings-of-lines)
        ("e" . #'mc/edit-ends-of-lines)
        ("m" . #'mc/mark-all-like-this)
        ("s" . #'mc/mark-all-in-region)
        ("h" . #'mc/mark-all-dwim)
        ("w" . #'mc/mark-all-word-like-this)))

(use-package vundo
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)
  (global-set-key (kbd "C-x u") #'vundo))
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html
(use-package breadcrumb
  ;; disable breadcrumb whel using lsp-mode, because lsp have this feature already.
  :hook (lsp-mode . (lambda () (breadcrumb-mode 0)))
  :config
  (breadcrumb-imenu-crumbs)
  (breadcrumb-mode))

;; treemacs
(use-package treemacs
  :config
  (treemacs-git-mode 'deferred)
  :bind
  ("M-0" . treemacs-select-window))
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

;; window selection with ace-window
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window)))

(winner-mode +1)

(use-package visual-fill-column
  :ensure (visual-fill-column :host github :repo "joostkremers/visual-fill-column" :inherit nil))

(use-package writeroom-mode
  :ensure (writeroom-mode :host github :repo "joostkremers/writeroom-mode" :inherit nil))

(use-package polymode)

(use-package envrc
  :hook (after-init . envrc-global-mode))

(provide 'init-editor)
;;; init-editor.el ends here
