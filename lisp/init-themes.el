;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

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

;; Themes
;; Great looking theme
(use-package spacemacs-theme
  :config (load-theme 'spacemacs-dark t))

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
  :hook (on-first-input . mode-line-bell-mode))
(use-package nyan-mode
  :config (nyan-mode))
;; A fancy and fast mode-line inspired by minimalism design.
;; doom-line not works will in terminal, use spaceline instead
(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; Initialization page
;; I don’t need a dashboard and I know where the manuals are. I prefer
;; a quiet startup.
(use-package "startup"
  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message nil))

;; frame scaling/zooming
(use-package default-text-scale
  :config
  (default-text-scale-mode))

;; Highlight the current line
(use-package hl-line
  :hook (on-first-buffer . global-hl-line-mode))

;; font
(when (display-graphic-p)
  (progn
    (set-face-attribute 'default nil :font "Source Code Pro" :weight 'normal)
    (set-fontset-font t 'han (font-spec :family "Droid Sans Fallback" :weight 'normal))
    ;; (set-fontset-font t 'kana (font-spec :family "Sarasa Gothic J" :weight 'normal :slant 'normal))
    (set-fontset-font t 'ascii (font-spec :family "Source Code Pro" :weight: 'normal :slant 'normal))))
(use-package all-the-icons
  :if (display-graphic-p))

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


(provide 'init-themes)
;;; init-themes.el ends here
