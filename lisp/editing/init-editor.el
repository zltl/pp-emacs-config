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
  :ensure (persist-state :type git :protocol https
                         :host github :repo "emacsmirror/persist-state"
                         :inherit nil)
  :defer t
  :diminish
  :init
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              ;; Optional `require' allows the editor core to load even if
              ;; this persistence helper is not installed yet.
              (when (require 'persist-state nil t)
                (persist-state-mode 1)))))

;; Newline at end of file
;; Keeping a trailing newline matches Unix conventions and prevents noisy
;; diffs in tools that expect text files to end with one.
(setq require-final-newline t)

;; This mode saves our place for when we revisit a file.
(use-package saveplace
  :ensure nil
  :hook (on-first-buffer . save-place-mode)
  :config
  (setq save-place-file (expand-file-name "var/.saved-places" user-emacs-directory)))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
;; `super-save' reduces the mental overhead of "did I save before
;; switching context?" while still respecting file-backed buffers only.
(use-package super-save
  :defer 1
  :diminish
  :config
  (super-save-mode +1)
  ;; Saving on idle protects work during long editing sessions and makes
  ;; external tools see fresher file contents.
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
;; `uniquify' is built in; requiring it here enables disambiguated buffer
;; names without pulling in another external package.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      ;; Copying backups avoids clobbering symlinks or hardlinks owned by
      ;; the original file, which is safer in dotfiles and deployment repos.
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
;; This matches modern editor behavior and makes region-based replacement
;; feel natural instead of requiring a manual delete first.
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
;; Respect per-project formatting rules so indentation, line endings, and
;; whitespace defaults follow the repository instead of global preferences.
(use-package editorconfig
  :defer t
  :diminish
  :init
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              ;; Enable EditorConfig only when the package is available so
              ;; formatting support degrades gracefully on fresh installs.
              (when (require 'editorconfig nil t)
                (editorconfig-mode 1)))))

;; Smarter deletion removes contiguous whitespace chunks in one go, which
;; is especially handy when cleaning indented code or aligned text.
(use-package smart-hungry-delete
  :defer t
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
 	       ([remap delete-backward-char] . smart-hungry-delete-backward-char)
 	       ([remap delete-char] . smart-hungry-delete-forward-char))
  :init
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              ;; The package is optional; only wire its default hooks when
              ;; the dependency is actually present.
              (when (require 'smart-hungry-delete nil t)
                (smart-hungry-delete-add-default-hooks)))))

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
;; Saving immediately avoids losing bookmark changes in daemon sessions or
;; after crashes where kill-emacs hooks never get the chance to run.
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1))

;; Automatically insert closing parens
;; Auto-pairing removes a lot of repetitive delimiter typing in code and prose.
(electric-pair-mode t)
;; Enable electric indent mode for auto-indentation on RET
;; Pressing RET should usually continue the current structure's indentation.
(electric-indent-mode t)
;; Visualize matching parens
;; Matching-paren feedback prevents subtle delimiter mistakes in nested code.
(show-paren-mode 1)
;; Prefer spaces to tabs (also set via ltl/use-tabs in init-custom-vars.el)
;; Note: save-place-mode is configured above with use-package
;; Save history in minibuffer to keep recent commands easily accessible
;; Persisting minibuffer/search history makes long-running Emacs sessions
;; feel stateful and reduces retyping for recurring commands and regexes.
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
;; This duplicates the earlier auto-revert intent intentionally so the
;; core editing module remains self-contained even if file tooling changes.
(global-auto-revert-mode t)

;; `smartparens' adds structural editing commands beyond plain auto-pairs:
;; slurp/barf, sexp navigation, unwrap, and context-aware pair handling.
(use-package smartparens
  :ensure t
  :hook (prog-mode text-mode markdown-mode)
  :config
  ;; `smartparens-config' ships sensible language defaults so we get
  ;; mature pairing rules without hand-maintaining each language table.
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
;; `multiple-cursors' is ideal for repetitive edits across similar lines
;; where regex replace would be overkill and plain macros too fragile.
(use-package multiple-cursors
  :defer t
  :bind
  (:map ltl/multicursor
        ("l" . #'mc/edit-lines)
        ("a" . #'mc/edit-beginnings-of-lines)
        ("e" . #'mc/edit-ends-of-lines)
        ("m" . #'mc/mark-all-like-this)
        ("s" . #'mc/mark-all-in-region)
        ("h" . #'mc/mark-all-dwim)
        ("w" . #'mc/mark-all-word-like-this)))

;; `vundo' visualizes the undo tree, which makes branching history easier
;; to understand than repeated plain undo/redo commands.
(use-package vundo
  :defer t
  :bind (("C-x u" . vundo))
  :custom
  ;; Take less on-screen space.
  (vundo-compact-display t))
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html
;; `breadcrumb' shows the current symbol path, which is helpful in deeply
;; nested code and complements imenu/LSP navigation.
(use-package breadcrumb
  :defer t
  ;; disable breadcrumb whel using lsp-mode, because lsp have this feature already.
  ;; :hook (lsp-mode . (lambda () (breadcrumb-mode 0)))
  :init
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              ;; Breadcrumb is a convenience layer, so load it opportunistically
              ;; without making the whole editor module depend on it.
              (when (require 'breadcrumb nil t)
                (breadcrumb-mode 1)))))

;; treemacs
;; `treemacs' provides a persistent project tree when a sidebar is more
;; efficient than repeated find-file/project commands.
(use-package treemacs
  :custom
  ;; Allow temporary width tweaks instead of forcing a fixed sidebar width.
  (treemacs-width-is-initially-locked nil)
  :config
  ;; Deferred Git state keeps the tree responsive in large repositories.
  (treemacs-git-mode 'deferred)
  :bind
  ("M-0" . treemacs-select-window))
;; Match Treemacs icons to the rest of the nerd-icons-based UI.
(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

;; window selection with ace-window
;; `ace-window' gives fast visual window targeting and scales better than
;; repeated C-x o in multi-window layouts.
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window)))

;; `winner-mode' makes window layout changes easy to undo/redo, which is
;; invaluable after temporary splits, Magit popups, or help buffers.
(winner-mode +1)

;; `visual-fill-column' narrows visual text width while keeping the
;; underlying buffer unchanged, ideal for prose and presentation modes.
(use-package visual-fill-column
  :ensure (visual-fill-column :type git :protocol https
                              :host github :repo "joostkremers/visual-fill-column"
                              :inherit nil)
  :defer t)

;; `writeroom-mode' builds on visual fill to create a distraction-free
;; writing environment when coding UI chrome becomes a liability.
(use-package writeroom-mode
  :ensure (writeroom-mode :type git :protocol https
                          :host github :repo "joostkremers/writeroom-mode"
                          :inherit nil)
  :defer t)

;; `polymode' supports buffers that mix multiple languages, such as
;; literate docs or templating files with embedded source blocks.
(use-package polymode
  :defer t)

;; repeat-mode - Repeat last command with single key (Emacs 28+)
;; Repeat mode turns families of commands into compact key sequences, so
;; repeated window/tab/navigation operations require less chording.
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode))

;; indent-bars - Visual indentation guides
;; Indentation guides are helpful in deeply nested code, especially when
;; whitespace is significant or long functions mix multiple scopes.
(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :custom
  ;; Prefer real characters over pixel bars so rendering stays crisp in
  ;; terminals and mixed-font setups.
  (indent-bars-prefer-character t))

;; `envrc' imports per-project environment variables from .envrc, which
;; keeps language servers, compilers, and tests aligned with the shell.
(use-package envrc
  :hook (after-init . envrc-global-mode))

(provide 'init-editor)
;;; init-editor.el ends here
