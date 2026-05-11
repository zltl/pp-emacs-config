;;; init-themes.el --- UI, themes, and visual configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This module configures the visual appearance of Emacs including:
;; - Color themes
;; - Fonts and icons
;; - Mode line
;; - Window chrome (toolbar, scrollbar, etc.)
;;
;;; Code:

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
;; The practical effect is that a curated theme list loads cleanly on a
;; new machine without interactive trust prompts during startup.
(setq custom-safe-themes t)

;; I like tiled windows more than I need Emacs to maintain a static
;; number of columns and rows.
;; Preventing implied resize avoids jarring frame jumps when fonts or UI
;; elements change size during startup.
(setopt frame-inhibit-implied-resize t)
;; Cursor
;; I like a non-blinking bar cursor.
(setopt cursor-type 'bar)
(blink-cursor-mode -1)

;; Mode line
;; Column number
(column-number-mode)

;; Scroll bars #
;; The mode line tells us where we’re at, and we mostly eschew the mouse.
(scroll-bar-mode -1)

;; Tool bars
;; The much despised tool bar is not a terrible default for the Emacs
;; neophyte, but I’m old and grizzled.
(tool-bar-mode -1)

;; menu bar
;; remove menu bar
(menu-bar-mode -1)

;; Themes
;; Great looking theme
;; `spacemacs-theme' is the primary visual baseline; loading it here means
;; all later UI packages inherit a consistent palette.
(use-package spacemacs-theme
  :config (load-theme 'spacemacs-dark t))
;; (use-package modus-themes
;;   :config (load-theme 'modus-vivendi))
;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (load-theme 'doom-one t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-neotree-config)
;;   ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; Minimization: let’s not {#minimization-let’s-not} #
;; I don’t much care for minimizing windows in the first place, and
;; particularly not my favorite window with a keybinding that’s too
;; easy to hit.
(global-set-key (kbd "C-z") nil)

;; Mac has an anoying bug when visibly warning you about errors. I hate it.
;; And while we're at it, let's ask emacs to ignore the audible warning too.
;; Disabling both visible and audible bells makes errors less disruptive,
;; especially during repetitive navigation or completion mistakes.
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Beep beep, your ass
;; Decades ago, there was a meme of Wile E. Coyote, having finally
;; caught Road Runner, saying “Beep beep your ass.” This comes from
;; approximately the same era as the last time anyone wanted a system
;; bell.
;; `mode-line-bell' keeps error feedback visible but subtle by flashing
;; the mode line instead of ringing the system bell.
(use-package mode-line-bell
  :hook (on-first-input . mode-line-bell-mode))
;; `nyan-mode' is mostly aesthetic; it adds a playful progress indicator
;; to the mode line without affecting editing behavior.
(use-package nyan-mode
  :config (nyan-mode))
;; A fancy and fast mode-line inspired by minimalism design.
;; doom-line not works will in terminal, use spaceline instead
;; (use-package spaceline
;;   :config
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme))

;; Initialization page
;; I don’t need a dashboard and I know where the manuals are. I prefer
;; a quiet startup.
(setf inhibit-splash-screen t)
(setf initial-major-mode 'fundamental-mode)
(setf initial-scratch-message nil)

;; frame scaling/zooming
;; `default-text-scale' provides persistent global zooming so different
;; displays can be accommodated without editing font constants.
(use-package default-text-scale
  :config
  (default-text-scale-mode))

;; Highlight the current line
;; A subtle current-line highlight improves cursor tracking in large files
;; and multi-pane layouts.
(use-package hl-line
  :ensure nil
  :hook (on-first-buffer . global-hl-line-mode))

;; `volatile-highlights' briefly highlights changed text, making actions
;; like yank, undo, or replace easier to visually verify.
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

;; font — uses defcustom variables from init-custom-vars.el
;; Apply fonts only in GUI sessions; terminal Emacs cannot use these font
;; faces and would just produce redundant work.
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :font ltl/default-font
                      :weight 'normal
                      :height ltl/font-size)
  (set-fontset-font t 'han
                    (font-spec :family ltl/chinese-font
                               :weight 'normal
                               :height ltl/font-size)))

;; Helper for icon/font packages so we can install missing fonts only when
;; necessary instead of prompting on every startup.
(defun ltl/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name)) t nil))

;; `all-the-icons' supplies icon glyphs for UI packages like dashboard or
;; dired integrations, but only makes sense in graphical Emacs.
(use-package all-the-icons
  :if (display-graphic-p)
  :defer t
  :config
  ;; Auto-install the icon font on first use so icon-dependent packages do
  ;; not silently render blank squares.
  (unless (ltl/font-installed-p "all-the-icons")
    (all-the-icons-install-fonts t)))
;; `nerd-icons' has broader ecosystem support than all-the-icons, so this
;; config keeps both available for packages that prefer one or the other.
(use-package nerd-icons
  :defer t)
;; Prefer Nerd Font-backed glyphs where possible because a single patched
;; font can cover more packages with fewer separate font installs.
(use-package all-the-icons-nerd-fonts
  :ensure
  (all-the-icons-nerd-fonts :host github :repo "mohkale/all-the-icons-nerd-fonts")
  :after all-the-icons
  :config
  (all-the-icons-nerd-fonts-prefer))

;; Menu
;; Dialog boxes are an unemacsian abomination.
(setopt use-dialog-box nil)

;; Mouse
;; I don’t use the mouse much in Emacs, but if I do, it’s the scroll
;; wheel. This makes it feel less like a trip back to a time before
;; scroll wheels.
(pixel-scroll-precision-mode)

;; `doom-modeline' offers a dense but readable modeline with project,
;; VCS, diagnostics, and buffer metadata in predictable positions.
(use-package doom-modeline
  :custom
  ;; Keep the bar/modeline compact so it is informative without dominating
  ;; vertical space on laptop displays.
  (doom-modeline-height 18)
  (doom-modeline-bar-width 8)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes
   '(markdown-mode gfm-mode org-mode rst-mode latex-mode tex-mode text-mode))
  :hook (after-init . doom-modeline-mode)
  :config
  ;; HACK: Add some padding to the right
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info
            remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus
                  github debug repl lsp minor-modes input-method indent-info buffer-encoding
                  major-mode process vcs time "  ")))

;; `solaire-mode' subtly differentiates "real" editing buffers from side
;; buffers, which improves window hierarchy at a glance.
(use-package solaire-mode
  :config
  (solaire-global-mode +1)
  (dolist (face '(mode-line mode-line-active mode-line-inactive mode-line-emphasis))
    (setf (alist-get face solaire-mode-remap-alist) nil)))

;; Tab-bar - Built-in tab management for multi-project workflows
;; Built-in tab-bar gives lightweight workspaces without another package
;; dependency, which is enough for separating projects/tasks visually.
(use-package tab-bar
  :ensure nil
  :custom
  ;; Always show the tab bar when multiple contexts are in use.
  (tab-bar-show 1)
  ;; Hide close buttons to reduce accidental mouse-driven tab deletion.
  (tab-bar-close-button-show nil)
  ;; New tabs should open in *scratch* so they start from a neutral buffer.
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-hints t)
  ;; Meta+number-style selection scales well for keyboard-driven tab switching.
  (tab-bar-select-tab-modifiers '(meta))
  :bind
  (("C-x t n" . tab-bar-new-tab)
   ("C-x t k" . tab-bar-close-tab)
   ("C-x t o" . tab-bar-switch-to-tab)
   ("C-<tab>" . tab-bar-switch-to-next-tab)
   ("C-S-<tab>" . tab-bar-switch-to-prev-tab)))

(provide 'init-themes)
;;; init-themes.el ends here
