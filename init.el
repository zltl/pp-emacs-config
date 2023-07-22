;;; init.el -- initial config for emacs
;;; Commentary:

;; A reasonable eamcs config.


;;; Code:

;;; Early Tasks

;;; For Chinese user, elpa may blocked by the Great-Fucking-Wirewall
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; A quick primer on the `use-package' function (refer to
;; "C-h f use-package" for the full details).
;;
;; (use-package my-package-name
;;   :ensure t    ; Ensure my-package is installed
;;   :after foo   ; Load my-package after foo is loaded (seldom used)
;;   :init        ; Run this code before my-package is loaded
;;   :bind        ; Bind these keys to these functions
;;   :custom      ; Set these variables
;;   :config      ; Run this code after my-package is loaded



;;; Startup time

;; Benchmark
;; benchmark-init is a simple package that may or may not carry its
;; weight versus usepackage-compute-statistics. Run
;; benchmark-init/show-durations-tabulated to check this one out.
(use-package benchmark-init
  :ensure t
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

;;;;; {

;; put packages init here

;; Unless we've already fetched (and cached) the package archives,
;; refresh them.
(unless package-archive-contents
  (package-refresh-contents))

;; Primitive package-vc integration for use-package
;; you may need a proxy on Chinese mainland for this
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)
;;;;; }

;;; Garbage collection
;; Increasing the garbage collector threshold is reputed to help at
;; init. After startup, we revert on the Garbage Collector Magic Hack.
(use-package gcmh
  :ensure t
  :vc (:fetcher github :repo emacsmirror/gcmh)
  :diminish
  :init (setq gc-cons-threshold (* 80 1024 1024))
  :hook (emacs-startup . gcmh-mode))

(setq read-process-output-max (* 1024 1024))

;;; Doom-like hooks
;; We’re also going to use on.el to provide some of the same hooks
;; Doom uses.
(use-package on
  :vc (:fetcher github :repo ajgrf/on.el)
  :ensure t)

;;; Security
;; For the love of all that is holy, do not continue with untrusted
;; connections!
(use-package gnutls
  :defer t
  :custom
  (gnutls-verify-error t))

;;; No littering
;; Many packages leave crumbs in user-emacs-directory or even
;; $HOME. Finding and configuring them individually is a hassle, so we
;; rely on the community configuration of no-littering. Run this
;; early, because many of the crumb droppers are configured below!
(use-package no-littering
  :ensure t
  :init
  (setq no-littering-etc-directory "~/.cache/emacs/etc/"
        no-littering-var-directory "~/.cache/emacs/var/"))
(require 'recentf)
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))

;; Frames
;; I like tiled windows more than I need Emacs to maintain a static
;; number of columns and rows.
(setopt frame-inhibit-implied-resize t)

;; Cursor
;; I like a non-blinking bar cursor.

(setopt cursor-type 'bar)
(use-package frame
  :config
  (blink-cursor-mode -1))

;; Mode line
;; Column number
(use-package simple
  :hook
  (on-first-buffer . column-number-mode))

;; Scroll bars #
;; The mode line tells us where we’re at, and we mostly eschew the mouse.
(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

;; Tool bars
;; The much despised tool bar is not a terrible default for the Emacs
;; neophyte, but I’m old and grizzled.
(use-package tool-bar
  :config
  (tool-bar-mode -1))

;; menu bar
;; remove menu bar
(menu-bar-mode -1)

;; Set the font. Note: height = px * 100
(set-face-attribute 'default nil :font "Consolas" :height 120)

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
      ;; I'll add an extra note here since user customizations are important.
      ;; Emacs actually offers a UI-based customization menu, "M-x customize".
      ;; You can use this menu to change variable values across Emacs. By default,
      ;; changing a variable will write to your init.el automatically, mixing
      ;; your hand-written Emacs Lisp with automatically-generated Lisp from the
      ;; customize menu. The following setting instead writes customizations to a
      ;; separate file, custom.el, to keep your init.el clean.
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; use-package keywords

;; bind-key
;; use-package is built-in as of Emacs 29, but since we use :bind, we
;; need to load bind-key. If we forget, we get the error: Symbol's
;; value as variable is void: personal-keybindings.
(use-package bind-key
  :demand t
  :bind
  (:prefix-map ltl/files-map
               :prefix "C-c f")
  :bind
  (:prefix-map ltl/toggles-map
               :prefix "C-c t")
  :bind
  (:prefix-map ltl/goto
               :prefix "C-c j")
  :config
  (defun rab/unbind-all (fn)
    "Unbinds a function everywhere."
    (dolist (key (where-is-internal fn nil))
      (unbind-key key))))

;; avy is a GNU Emacs package for jumping to visible text using a
;; char-based decision tree
(use-package avy
  :ensure t
  :bind
  (:map ltl/goto
        ("c" . #'avy-goto-char)))

;; Diminish
;; We also want to “diminish” most minor-mode indicators on the mode
;; line. They’re only interesting if they’re in an unexpected state.
(use-package diminish :ensure t)

;;; Path setup
;; Launching Emacs from the MacOS dock does not source my shell
;; config, which leads to my Nix profile not being on the $PATH, which
;; leads to errors, or worse, trying to install the execrable Xcode.
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))


;;;
;;; General customization
;;;

;;; Editing

;;; Editing basics

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

;; Mark ring
;; set-mark-command-repeat-pop means we only need to hit C-u or C-x
;; once before subsequent C-SPC, which makes it much nicer to
;; navigate.
(setopt set-mark-command-repeat-pop t)

;; Indent
;; Tabs are the devil’s whitespace.
(use-package simple
  :config
  (setq-default indent-tabs-mode nil))

;; Killing
;; Put the clipboard on the kill ring before killing something
;; else. Emacs isn’t as violent as it sometimes sounds, I swear.
;;
;; We also don’t want to clutter the ring with consecutively duplicate
;; values.
(use-package simple
  :custom
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t))

;;; Matching

;; Bookmark
;; Persist bookmarks each time we set one, not when Emacs exits.
(use-package bookmark
  :custom
  (bookmark-save-flag 1))


;;;
;;; Convenience
;;;

;;; Completion

;; copilot
;; I think Copilot’s training was unethical, and I’m skeptical of its
;; utility, but I need to get some experience with it.

;; always in copilot-disable-predicates turns off automatic
;; completion. We can still reach it from M-`, which is chosen to be
;; close to M-TAB and bound to a menubar command I don’t ever use.

;; TODO: use M-x copilot-login
(use-package copilot
  :vc (:fetcher github :repo zerolfx/copilot.el)
  :ensure t
  :custom
  (copilot-disable-predicates '(always))
  :hook
  (prog-mode . copilot-mode)
  :bind
  ("M-`" . copilot-complete)
  :bind
  (:map ltl/toggles-map
   ("`" . copilot-mode))
  :bind
  (:map copilot-completion-map
   ("C-g" .  #'copilot-clear-overlay)
   ("M-p" . #'copilot-previous-completion)
   ("M-n" . #'copilot-next-completion)
   ("TAB" . #'copilot-accept-completion)
   ("yy" . #'copilot-accept-completion)
   ("M-f" . #'copilot-accept-completion-by-word)
   ("M-<return>" . #'copilot-accept-completion-by-line)))

;; Corfu complete ui
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  ;; You may want to play with delay/prefix/styles to suit your preferences.
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (completion-styles '(basic)))


;; Exiting
;; I’d usually rather exit Slack, to be quite honest.
(setopt confirm-kill-emacs 'yes-or-no-p)


;; Highlight the current line
(use-package hl-line
  :hook (on-first-buffer . global-hl-line-mode))

;; ffap
;; ffap, short for “find file at point,” guesses a default file from
;; the point. ffap-bindings rebinds several commands with ffap
;; equivalents.
(use-package ffap
  :hook (on-first-input . ffap-bindings))

;; Persist state
;; Persist State flushes state that is normally flushed in
;; kill-emacs-hook, which I’m trying not to call until I die.
(use-package persist-state
  :ensure t
  :hook
  (on-first-input . persist-state-mode))

;; Whitespace butler
(use-package ws-butler
  :ensure t
  :hook (on-first-buffer . ws-butler-global-mode)
  :diminish)


;;;
;;; Files
;;;

;; Lock files
;; On single-user environments, as we tend to run Emacs these days,
;; those .#* files are more likely to confuse some other program as
;; they are to protect us from conflicting edits.
(setopt create-lockfiles nil)

;; Auto-revert
(use-package autorevert
  :diminish auto-revert-mode
  :hook (on-first-buffer . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t))

;; Recent files
;; This maintains a list of recent files, as we often find in other
;; applications. I wonder if it can or should be integrated with
;; MacOS' list of recent files?
(use-package recentf
  :hook (on-first-file-hook . recentf-mode)
  :bind
  (:map ltl/files-map
   ("r" . recentf-open)))

(use-package project
  :ensure t
  :config
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))
  :config
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))
  :config
  (add-hook 'project-find-functions #'project-find-go-module))



;;;
;;; Text
;;;

;;; Case

;; DWIM case
;; These do-what-I-mean bindings are newer than the classic
;; keybindings, but a better default.
(use-package emacs
  :bind
  ([remap capitalize-word] . capitalize-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap upcase-word] . upcase-dwim))

;; Title case
;; Gosh, I wish I’d had this when I was so active on MusicBrainz.
(use-package titlecase
  :ensure t
  :defer t)

;; Jinx
;; Jinx is a just-in-time spell checker.
(use-package jinx
  :ensure t
  :hook (on-first-buffer . global-jinx-mode)
  :bind
  ([remap ispell-word] . jinx-correct)
  :bind
  (:map ltl/toggles-map
   ("$" . jinx-mode)))

;;; Outlines
;; Org
;; Org Mode’s timestamps are sadly not aware of time zones, but we can
;; crudely approximate support by setting org-time-stamp-formats.
(use-package org
  :config
  (setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M %Z>"))
  (setf org-startup-folded 'show2levels))
(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda ()  org-bullets-mode 1)))

;; ox-hugo
;; We use ox-hugo for publishing.
;;
;; ltl/ox-hugo-update-lastmod can be used to update the timestamp of
;; the exported tree at the current point.
(use-package ox-hugo
  :ensure t
  :after org
  :config
  (defun ltl/ox-hugo-update-lastmod ()
    "Updates the EXPORT_HUGO_LAST_MOD property of the nearest element
with EXPORT_FILE_NAME."
    (interactive)
    (save-excursion
      (when-let* ((elem (car (org-hugo--get-elem-with-prop :EXPORT_FILE_NAME)))
                  (begin (org-element-property :begin elem))
                  (time (format-time-string (org-time-stamp-format t) (current-time))))
        (org-entry-put begin "EXPORT_HUGO_LASTMOD" time)))))

;; ox-slack
;; Mostly useful for org-slack-export-to-clipboard-as-slack.
(use-package ox-slack
  :ensure t
  :after org
  :bind
  (:map org-mode-map
   :prefix-map rab/org-mode-map
   :prefix "C-c m"
   ("w" . org-slack-export-to-clipboard-as-slack)))

;; `org-mode' is great but Denote makes it even better by adding
;; features that you'd find in something like Obsidian (like
;; backlinks!). You can write your notes in org, markdown, or plain
;; text, though I recommend giving `org-mode' a try if you've never
;; used it before. The Denote manual is also excellent:
;; https://protesilaos.com/emacs/denote
(use-package denote
  :ensure t
  :custom
  (denote-known-keywords '("emacs" "journal"))
  ;; This is the directory where your notes live.
  (denote-directory (expand-file-name "~/denote/"))
  :bind
  (("C-c n n" . denote)
   ("C-c n f" . denote-open-or-create)
   ("C-c n i" . denote-link)))


;; Subword mode
;; Subword mode helps us move around camel-case languages, and is
;; mostly configured as a hook in those major modes. The only thing we
;; customize about it is not wanting it cluttering the mode line.
(use-package subword
  :defer t
  :diminish)

;; Counting words
;; The default binding of M-= is count-words-region. The newer
;; count-words counts the buffer when there’s no active region.
(bind-key [remap count-words-region] 'count-words)


;;;
;;; Data
;;;

;; Save place
;; This mode saves our place for when we revisit a file.
(use-package saveplace
  :hook (on-first-buffer . save-place-mode))


;;;
;;; External
;;;

;; RFC mode
(use-package rfc-mode
  :ensure t
  :defer t)

;; Envrc
;; I maintain a minimal home environment and push as much as I can to
;; Nix flakes. This insulates me from conflicting dependencies, makes
;; my projects more portable, and helps me share with Nix-enabled
;; teammates.
;;
;; Where possible, I add an .envrc file to load the environment from the flake.
(use-package envrc
  :ensure t
  :hook (on-first-file . envrc-global-mode))


;;;
;;; Programming
;;;

;; Automatically insert closing parens
(electric-pair-mode t)
;; Visualize matching parens
(show-paren-mode 1)
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

;; Code completion at point
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;;; Languages

;; C#
;; I am not a C# developer, but I’ve been known to interview them.
(use-package csharp-mode
  :mode ((rx ".cs" eos) . 'csharp-ts-mode)
  :hook (csharp-ts-mode . subword-mode))

;; Markdown
(use-package markdown-mode
  :ensure t)

;; compilation
;; I get a bunch of asynchronous warnings from native compilation in a
;; *Warnings* popup. It’s nice that they’re there, but unless they’re
;; an error, I don’t need them all up in my business.
(use-package comp
  :custom
  (native-comp-async-report-warnings-errors 'silent))

;; This Emacs library provides a global mode which displays ugly form
;; feed characters as tidy horizontal rules.
;;
;; I use ^L to break sections on lisp
(use-package page-break-lines
  :ensure t
  :hook  ((lisp-mode . page-break-lines-mode)
          (emacs-lisp-mode . page-break-lines-mode)))

(use-package sly
  :ensure t)

;; Nix
(use-package nix-mode
  :ensure t
  :defer t)

;; XML
;; esxml essentially turns Lisp into an XML (or XHTML) templating
;; engine.
(use-package esxml
  :ensure t
  :defer t)

;; YAML
(use-package yaml-mode
  :ensure t
  :defer t)

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
  :ensure t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

;; multi cursor
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-c C-c") 'mc/edit-lines)

  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; color parens
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#e91e63"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#2196F3"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#EF6C00"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#B388FF"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#76ff03"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#26A69A"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#FFCDD2"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "#795548"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "#DCE775"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "#FFFFFF" :background "#EF6C00"))))
   '(whitespace-tab ((t (:foreground "#636363"))))))

(use-package highlight-thing
  :ensure t
  :hook (prog-mode . highlight-thing-mode))

(use-package elixir-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
	      ("C-c C-f" . 'gofmt)))
;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)


(use-package julia-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

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

;; Note that `php-mode' assumes php code is separate from HTML.
;; If you prefer working with PHP and HTML in a single file you
;; may prefer `web-mode'.
(use-package php-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :bind (:map rust-mode-map
	      ("C-c C-r" . 'rust-run)
	      ("C-c C-c" . 'rust-compile)
	      ("C-c C-f" . 'rust-format-buffer)
	      ("C-c C-t" . 'rust-test))
  :hook (rust-mode . prettify-symbols-mode))

;; TypeScript, JS, and JSX/TSX support.
(use-package web-mode
  :ensure t
  :mode (("\\.ts\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.mjs\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-quoting nil))

(use-package clang-format
  :ensure t)

(use-package lsp-tailwindcss
  :ensure t
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package python-mode
  :ensure t)
(use-package anaconda-mode
  :ensure t
  :hook (python-mode . anaconda-mode))

;; lsp
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (web-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\target\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\deps\\'"))
;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


;;;
;;; Tools
;;;

;;; git

;; Magit
;; I have known people to leave Emacs, but continuing to use Magit for
;; version control. It’s that good.
;;
;; I am giving built-ins the benefit of the doubt in this config, and
;; would like to get into vc-mode. But I’m an advanced enough Git user
;; that something tailor-made carries its weight here.
(use-package magit
  :ensure t
  :defer 1
  :functions ltl/magit-clone-read-args-a
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-clone-default-directory "~/src/")
  (magit-no-message (list "Turning on magit-auto-revert-mode..."))
  (magit-save-repository-buffers 'dontask)
  :config
  (defun ltl/magit-clone-read-args-a (orig-fun &rest args)
    "Sets `vertico-preselect' to `prompt' when cloning repos, so we
clone to the default prompted directory, and not some random
existing directory under `magit-clone-default-directory'."
    (let ((vertico-preselect 'prompt))
      (apply orig-fun args)))
  (advice-add 'magit-clone-read-args :around #'rab/magit-clone-read-args-a))

;; Git-Link
;; git-link grabs links to lines, regions, commits, or home pages.
(use-package git-link
  :ensure t
  :custom
  (git-link-use-commit t)
  (git-link-use-single-line-number t)
  :commands (git-link git-link-commit git-link-homepage))

;; Git-Related
;; git-related sorts files in a project by a similarity score derived
;; from how often they change in the same commit.
(use-package git-related
  :bind
  (:map ltl/files-map
   ("g" . git-related-find-file)))

;; Treesitter
;; treesit-auto finds treesitter modes by naming convention.
(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (global-treesit-auto-mode))

;; UUID Generation
(use-package uuidgen
  :ensure t
  :defer t)


;;;
;;; Applications
;;;

;; Dictionary
;; The M-# keybinding is dubious because it’s not reserved, but it’s
;; good enough for Mickey Petersen.
(use-package dictionary
  :bind
  ("M-#" . dictionary-lookup-definition))
;; Until I find a working dictd for MacOS on Nix, we’ll sigh heavily
;; and use dict.org.
(use-package dictionary
  :if (memq window-system '(mac ns x))
  :custom
  (dictionary-server "dict.org"))


;;;
;;; Development
;;;

;; htmlize
;; htmlize provides syntax highlighting for our code snippets when
;; exported to HTML.
(use-package htmlize
  :ensure t
  :after ox-html)


;;;
;;; Environment
;;;

;; Dired
;; Dired should refresh the listing on each revisit.
(use-package dired
  :defer
  :custom
  (dired-auto-revert-buffer t))

;; Minimization: let’s not {#minimization-let’s-not} #
;; I don’t much care for minimizing windows in the first place, and
;; particularly not my favorite window with a keybinding that’s too
;; easy to hit.
(use-package frame
  :bind
  ("C-z" . nil))

;; Beep beep, your ass
;; Decades ago, there was a meme of Wile E. Coyote, having finally
;; caught Road Runner, saying “Beep beep your ass.” This comes from
;; approximately the same era as the last time anyone wanted a system
;; bell.
(use-package mode-line-bell
  :ensure
  :hook (on-first-input . mode-line-bell-mode))

(use-package nyan-mode
  :ensure t
  :config (nyan-mode))

;; Themes
;; Great looking theme
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi :no-confirm))

;; undo-tree
(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

;; Breadcrumb adds, well, breadcrumbs to the top of your open buffers
;; and works great with project.el, the Emacs project manager.
;;
;; Read more about projects here:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html
(use-package breadcrumb
  :vc (:fetcher github :repo joaotavora/breadcrumb)
  :init (breadcrumb-mode))

;; A fancy and fast mode-line inspired by minimalism design.
;; doom-line not works will in terminal, use spaceline instead
;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1))
(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; Initialization
;; I don’t need a dashboard and I know where the manuals are. I prefer
;; a quiet startup.
(use-package "startup"
  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message nil))

;; Marginalia
;; Marginalia annotates minibuffer completions with some useful info.
(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult
;; Consult provides several enhanced functions for completing-read. It
;; fits nicely with Vertico.
;;
;; I generally remapped everything
;; obvious. consult-yank-from-kill-ring as a remapping of yank proved
;; a bit too disorienting.
(use-package consult
  :ensure t
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap project-switch-to-buffer] . consult-project-buffer)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap recentf-open] . consult-recent-file)
  ([remap yank] . nil)
  ([remap yank-pop] . consult-yank-pop)
  ([remap goto-line] . consult-goto-line)
  ("M-g m" . consult-mark)
  ("M-g M" . consult-global-mark)
  ("M-g o" . consult-outline)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ("M-s r" . consult-ripgrep)
  ("M-s f" . consult-find)
  ("M-s F" . consult-locate)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake)
  ([remap repeat-complex-command] . consult-complex-command)
  ("M-s e" . consult-isearch-history)
  ([remap isearch-edit-string] . consult-isearch-history)
  ([remap next-matching-history-element] . consult-history)
  ([remap previous-matching-history-element] . consult-history)
  ([remap Info-search] . consult-info)
  :custom
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :bind ("M-0" . treemacs-select-window))

;; better search in buffer
(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))

;; Menu
;; Dialog boxes are an unemacsian abomination.
(setopt use-dialog-box nil)

;; Mouse
;; I don’t use the mouse much in Emacs, but if I do, it’s the scroll
;; wheel. This makes it feel less like a trip back to a time before
;; scroll wheels.
(use-package pixel-scroll
  :hook
  (on-first-buffer . pixel-scroll-precision-mode))

;; Vertico
;; Vertico is a little bit nicer version of the builtin
;; icomplete-vertical.
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))


;; Vertico indexed
;; vertico-indexed lets us select candidates by number with C-u
;; RET. It’s an alternative to vertico-quick.
(use-package vertico-indexed
  :after vertico
  :config (vertico-indexed-mode))

;; Vertico repeat
;; vertico-repeat resumes a prior completion session.
(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-R" . vertico-repeat))

;; Vertico directory
;; vertico-directory does some smarter things when completing
;; directories:
;;
;; RET continues completing in that directory instead of jumping to
;; dired.
;;
;; M-DEL deletes whole directories at a time if the prompt ends in a
;; slash. There’s a recommended binding for DEL, but I’d rather keep
;; that deleting chars.
;;
;; I never understood vertico-directory-tidy before this demo. When we
;; start with / or ~/, it cleans up the leading default prompt that’s
;; “shadowed”.
(use-package vertico-directory
  :after vertico
  :bind
  (:map vertico-map
   ("RET" . vertico-directory-enter)
   ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
;; Vertico multiform
(use-package vertico-multiform
  :after vertico
  :custom
  (vertico-multiform-commands '((git-related-find-file (vertico-sort-function . nil))))
  :config
  (vertico-multiform-mode))

;; treemacs
(use-package treemacs
  :ensure t
  :config
  (treemacs-git-mode 'deferred))


;;;
;;; Windows
;;;

;; call other-window after splitting
(use-package window
  :config
  (defun ltl/nav-split-and-follow-below ()
    "Split the selected window in two with the new window is bellow.
This uses `split-window-below' but follows with the cursor."
    (interactive)
    (split-window-below)
    (other-window 1))

  (defun ltl/nav-split-and-follow-right ()
    "Split the selected window in two with the new window is to the right.
This uses `split-window-right' but follows with the cursor."
    (interactive)
    (split-window-right)
    (other-window 1))
  :bind
  ([remap split-window-below] . ltl/nav-split-and-follow-below)
  ([remap split-window-right] . ltl/nav-split-and-follow-right))


;;;
;;; Help
;;;

;; Which Key
;; which-key pops up a menu of keybindings. The traditional way is to
;; run it on a timer, but I prefer manual activation.
;;
;; I also relabel all my keymaps of the form ltl/blah-map to
;; blah. Neither :prefix-docstring nor :menu-item in bind-key seem to
;; do the trick.
(use-package which-key
  :ensure t
  :hook (on-first-input . which-key-mode)
  :diminish
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay most-positive-fixnum)
  (which-key-idle-secondary-delay 1e-9)
  :config
  (push `((nil . ,(rx bos "ltl/" (group (1+ any)) "-map" eos)) .
          (nil . ,(rx (backref 1))))
        which-key-replacement-alist))

;; C-h C-h shadows which-key with something less useful.
(use-package help
  :config
  (rab/unbind-all 'help-for-help))

;; Junk drawer
;;
;; These customizations don’t fit anywhere else.
;; Remove the training wheels #
(put 'narrow-to-region 'disabled nil)

;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings
;; recommended by the package author.
(use-package helpful
  :ensure t
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((copilot :vc-backend Git :url
              "https://github.com/zerolfx/copilot.el")
     (on :vc-backend Git :url "https://github.com/ajgrf/on.el")
     (gcmh :vc-backend Git :url "https://github.com/emacsmirror/gcmh")
     (vc-use-package :vc-backend Git :url
                     "https://github.com/slotThe/vc-use-package"))))
