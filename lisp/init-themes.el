;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package better-defaults)
(global-set-key (kbd "C-x C-f") #'find-file-at-point)
(global-set-key (kbd "C-x b") #'switch-to-buffer)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; I like tiled windows more than I need Emacs to maintain a static
;; number of columns and rows.
(setopt frame-inhibit-implied-resize t)
;; Cursor
;; I like a non-blinking bar cursor.
(setopt cursor-type 'bar)
(setf blink-cursor-mode -1)

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
(use-package default-text-scale
  :config
  (default-text-scale-mode))

;; Highlight the current line
(use-package hl-line
  :hook (on-first-buffer . global-hl-line-mode))

;; font
(when (display-graphic-p)
  (let ((font-size 100))
    (set-face-attribute 'default nil :font "Source Code Pro" :weight 'normal :height font-size)
    (set-fontset-font t 'han (font-spec :family "Droid Sans Fallback" :weight 'normal :height font-size))
    ;; (set-fontset-font t 'kana (font-spec :family "Sarasa Gothic J" :weight 'normal :slant 'normal))
    (set-fontset-font t 'ascii (font-spec :family "Source Code Pro" :weight: 'normal :slant 'normal :height font-size))))

(defun my/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name)) t nil))

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (unless (my/font-installed-p "all-the-icons")
    (all-the-icons-install-fonts t)))

(use-package nerd-icons)

;; Menu
;; Dialog boxes are an unemacsian abomination.
(setopt use-dialog-box nil)

;; Mouse
;; I don’t use the mouse much in Emacs, but if I do, it’s the scroll
;; wheel. This makes it feel less like a trip back to a time before
;; scroll wheels.
(pixel-scroll-precision-mode)

(use-package doom-modeline
  :custom
  (doom-modeline-height 18)
  (doom-modeline-bar-width 8)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes
   '(markdown-mode gfm-mode org-mode rst-mode latex-mode tex-mode text-mode))
  :config
  ;; HACK: Add some padding to the right
  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name window-number modals matches follow buffer-info
            remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus
                  github debug repl lsp minor-modes input-method indent-info buffer-encoding
                  major-mode process vcs checker time "  "))
  (doom-modeline-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1)
  (dolist (face '(mode-line mode-line-active mode-line-inactive mode-line-emphasis))
    (setf (alist-get face solaire-mode-remap-alist) nil)))


(provide 'init-themes)
;;; init-themes.el ends here
