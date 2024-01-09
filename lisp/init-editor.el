

;; Persist state
;; Persist State flushes state that is normally flushed in
;; kill-emacs-hook, which I’m trying not to call until I die.
(use-package persist-state
  :straight nil
  :diminish
  :config
  (persist-state-mode))

;; This mode saves our place for when we revisit a file.
(use-package saveplace
  :hook (on-first-buffer . save-place-mode))

;; auto-saving changed files
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
  :defer t
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
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Character radix
;; Make C-q read a hex sequence instead of the default octal. Obscure,
;; but I know more characters by their hex codes. This is also
;; consistent with C-x 8 <RET>, which is more chars, but offers
;; minibuffer completion.
(setopt read-quoted-char-radix 16)

;; Delete Selection Mode #
;; Typing over an active section should delete the section.
(use-package delsel
  :defer t
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

;; space between chinese and english
(use-package pangu-spacing
  :diminish
  :config
  ;; (global-pangu-spacing-mode 1)
)
(add-hook 'org-mode-hook
          #'(lambda ()
              (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))
(add-hook 'markdown-mode-hook
          #'(lambda ()
              (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

;; Persist bookmarks each time we set one, not when Emacs exits.
(use-package bookmark
  :custom
  (bookmark-save-flag 1))

;; Automatically insert closing parens
(electric-pair-mode t)
;; Visualize matching parens
(show-paren-mode 1)
(electric-pair-mode t)
;; Prefer spaces to tabs
(setq-default indent-tabs-mode nil)
;; Automatically save your place in files
(save-place-mode t)
;; Save history in minibuffer to keep recent commands easily accessible
(savehist-mode t)
;; Keep track of open files
(recentf-mode t)
;; Keep files up-to-date when they change outside Emacs
(global-auto-revert-mode t)

;; multi cursor
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-c C-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

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

(use-package writeroom-mode)

(provide 'init-editor)
