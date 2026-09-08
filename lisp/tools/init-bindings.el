;;; init-bindings.el --- Global key bindings configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module defines global key bindings for:
;; - Window management (ace-window, window navigation)
;; - Buffer operations
;; - Common editing commands
;; - Custom functions and utilities
;;
;; Uses which-key to display available bindings.
;;
;;; Code:

(declare-function ltl/agent--agent-window "init-agent-cli")
(declare-function ltl/agent--pane-width "init-agent-cli")
(declare-function treemacs-get-local-window "treemacs")
(declare-function treemacs-set-width "treemacs")

(defvar treemacs-width 35)

(autoload 'ltl/window-resize-menu "init-window-resize"
  "Show the persistent window resize menu." t)
(declare-function ltl/profile-cpu "init-perf")

;;;
;;; key bindings
;;;


;; windows
;; On Windows, remap the physical modifier keys so Emacs gets a richer
;; set of shortcuts without fighting system-level defaults too much.
(setq w32-apps-modifier 'hyper)
(setq w32-lwindow-modifier 'super)
(setq w32-rwindow-modifier 'hyper)

;; use-package is built-in as of Emacs 29, but its :bind support still
;; relies on bind-key. Load it before defining the shared prefix maps.
(use-package bind-key
  :ensure nil
  :demand t
  ;; Define named prefix maps once so later modules can hang commands off
  ;; a single C-c namespace:
  ;;   f files | t tools | j jump | p parens | m multicursor
  ;;   l lsp   | a gptel   | n notes | g git | h help
  ;; Agent uses `ltl/agent-command-map' on C-c c. Combobulate owns C-c o.
  :bind
  (:prefix-map ltl/files-map
               :prefix "C-c f")
  :bind
  (:prefix-map ltl/tools-map
               :prefix "C-c t")
  :bind
  (:prefix-map ltl/goto-map
               :prefix "C-c j")
  :bind
  (:prefix-map ltl/smartparen-map
               :prefix "C-c p")
  :bind
  (:prefix-map ltl/multicursor-map
               :prefix "C-c m")
  :bind
  (:prefix-map ltl/lsp-map
               :prefix "C-c l")
  :bind
  (:prefix-map ltl/ai-map
               :prefix "C-c a")
  :bind
  (:prefix-map ltl/notes-map
               :prefix "C-c n")
  :bind
  (:prefix-map ltl/git-map
               :prefix "C-c g")
  :bind
  (:prefix-map ltl/help-map
               :prefix "C-c h"))

;; use evil when "C-c t e"
;; Keep Evil opt-in instead of always on: this preserves vanilla Emacs
;; editing by default while still making modal editing one toggle away.
(use-package evil
  :bind
  (:map ltl/tools-map
        ("e" . evil-mode)))

;; Tool launchers/toggles share the C-c t namespace.
(bind-key "t" #'treemacs-select-window ltl/tools-map)
(bind-key "r" #'ltl/reload-config ltl/tools-map)
(bind-key "P" #'ltl/profile-cpu ltl/tools-map)

;; Window resizing is a property of the Emacs window, independent of
;; whether it displays a file, vterm, Treemacs, or an agent session.
(defun ltl/window--sync-treemacs-width ()
  "Keep `treemacs-width' aligned with the selected Treemacs window."
  (when (derived-mode-p 'treemacs-mode)
    (setq treemacs-width (window-body-width))))

(defun ltl/window-narrower (&optional count)
  "Shrink the selected window horizontally by COUNT columns.
Temporarily ignore `window-size-fixed' so a locked Treemacs sidebar can
still be nudged from the resize menu."
  (interactive "p")
  (let ((window-size-fixed nil))
    (shrink-window-horizontally count))
  (ltl/window--sync-treemacs-width))

(defun ltl/window-wider (&optional count)
  "Enlarge the selected window horizontally by COUNT columns.
Temporarily ignore `window-size-fixed' so a locked Treemacs sidebar can
still be nudged from the resize menu."
  (interactive "p")
  (let ((window-size-fixed nil))
    (enlarge-window-horizontally count))
  (ltl/window--sync-treemacs-width))

(defun ltl/window-shorter (&optional count)
  "Shrink the selected window vertically by COUNT lines."
  (interactive "p")
  (shrink-window count))

(defun ltl/window-taller (&optional count)
  "Enlarge the selected window vertically by COUNT lines."
  (interactive "p")
  (enlarge-window count))

(defun ltl/window-balance ()
  "Balance all windows on the selected frame."
  (interactive)
  (balance-windows))

(defun ltl/window--set-width (window width label)
  "Resize WINDOW toward WIDTH columns and report it as LABEL.
Respect normal minimum/fixed-size constraints.  Return the actual width."
  (unless (window-live-p window)
    (user-error "%s window is not visible" label))
  (unless (and (integerp width) (> width 0))
    (user-error "%s width must be a positive integer" label))
  ;; Treemacs (and similar sidebars) set buffer-local `window-size-fixed';
  ;; clear it only while measuring/resizing this WINDOW.
  (with-selected-window window
    (let* ((window-size-fixed nil)
           (delta (- width (window-width window)))
           (allowed (if (zerop delta)
                        0
                      (or (window-resizable window delta t) 0))))
      (when (and (not (zerop delta)) (zerop allowed))
        (user-error "%s window cannot be resized in the current layout" label))
      (unless (zerop allowed)
        (window-resize window allowed t))
      (let ((actual (window-width window)))
        (message "%s width: %d%s"
                 label actual
                 (if (= actual width)
                     ""
                   (format " (requested %d; layout limit)" width)))
        actual))))

(defun ltl/window-agent-preset (&optional width)
  "Resize the visible agent pane to its preset WIDTH.
Without a prefix argument use `ltl/agent-window-width', including its
TTY cap.  With a numeric prefix argument use that many columns."
  (interactive
   (list (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))))
  (unless (fboundp 'ltl/agent--agent-window)
    (user-error "Agent integration is not loaded"))
  (let ((window (ltl/agent--agent-window))
        (target (or width
                    (and (fboundp 'ltl/agent--pane-width)
                         (ltl/agent--pane-width))
                    100)))
    (ltl/window--set-width window target "Agent")))

(defun ltl/window-treemacs-preset (&optional width)
  "Resize Treemacs to its preset WIDTH.
Without a prefix argument use `treemacs-width'.  With a numeric prefix
argument update `treemacs-width' first."
  (interactive
   (list (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))))
  (unless (require 'treemacs nil t)
    (user-error "Treemacs is not installed"))
  (let ((window (treemacs-get-local-window)))
    (unless (window-live-p window)
      (user-error "Treemacs is not visible"))
    (when width
      (unless (> width 0)
        (user-error "Treemacs width must be a positive integer"))
      (setq treemacs-width width))
    ;; `treemacs-set-width'/`treemacs--set-width' resize the *selected*
    ;; window, so select Treemacs explicitly (C-c t w is usually pressed
    ;; from another window).
    (with-selected-window window
      (treemacs-set-width '(4)))
    (message "Treemacs width: %d" treemacs-width)))

(defvar-keymap ltl/window-resize-map
  :doc "Resize windows"
  "h" #'ltl/window-narrower
  "l" #'ltl/window-wider
  "j" #'ltl/window-shorter
  "k" #'ltl/window-taller
  "=" #'ltl/window-balance
  "a" #'ltl/window-agent-preset
  "t" #'ltl/window-treemacs-preset)

(bind-key "w" #'ltl/window-resize-menu ltl/tools-map)

;; Keep the resize map active after every command while `repeat-mode' is on.
(dolist (command '(ltl/window-narrower ltl/window-wider
                   ltl/window-shorter ltl/window-taller
                   ltl/window-balance ltl/window-agent-preset
                   ltl/window-treemacs-preset))
  (put command 'repeat-map 'ltl/window-resize-map))

(defun ltl/unbind-all (fn)
  "Unbinds a function everywhere."
  (dolist (key (where-is-internal fn nil))
    (unbind-key key)))

;; C-c 2 to start mark region
;; This gives region marking a dedicated mnemonic shortcut that is easy
;; to hit on keyboards where C-SPC may conflict with input methods.
(global-set-key (kbd "C-c 2")  #'set-mark-command)

;; avy is a GNU Emacs package for jumping to visible text using a
;; char-based decision tree
(use-package avy
  :config
  ;; Dim the background so the jump targets stand out immediately.
  (setq avy-background t)
  ;; Use full labels up front to reduce ambiguity when many candidates are visible.
  (setq avy-style 'at-full)
  :bind
  (:map ltl/goto-map
        ("c" . #'avy-goto-char)
        ("j" . #'avy-goto-word-0)
        ("l" . #'avy-goto-line)))

;; change all prompts to y or n
;; Short prompts remove a lot of friction in daily use without changing
;; what Emacs asks for; they only shorten the confirmation keystrokes.
(fset 'yes-or-no-p 'y-or-n-p)

;; Which Key
;; which-key pops up a menu of keybindings. The traditional way is to
;; run it on a timer, but I prefer manual activation.
;;
;; I also relabel all my keymaps of the form ltl/blah-map to
;; blah. Neither :prefix-docstring nor :menu-item in bind-key seem to
;; do the trick.
(use-package which-key
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
(ltl/unbind-all 'help-for-help)

;; Add descriptions for prefix maps
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements global-map
    "C-c f" "files"
    "C-c t" "tools"
    "C-c t r" "reload config"
    "C-c t P" "profile cpu"
    "C-c t t" "treemacs"
    "C-c t e" "evil"
    "C-c t v" "vterm"
    "C-c t m" "minuet auto"
    "C-c t $" "jinx"
    "C-c t a" "agent"
    "C-c t w" "resize windows"
    "C-c j" "goto"
    "C-c p" "smartparens"
    "C-c m" "multicursor"
    "C-c l" "lsp/eglot"
    "C-c c" "agent"
    "C-c a" "gptel"
    "C-c n" "notes"
    "C-c g" "git"
    "C-c h" "help"
    "C-c o" "combobulate"
    "C-c w" "workspaces"
    "C-c 2" "set-mark")
  (which-key-add-keymap-based-replacements ltl/window-resize-map
    "h" "narrower"
    "l" "wider"
    "j" "shorter"
    "k" "taller"
    "=" "balance"
    "a" "agent preset"
    "t" "treemacs preset"))

;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings
;; recommended by the package author.
;; `helpful' makes help buffers actually useful: source links, caller
;; info, and richer docs reduce the need to jump to raw Elisp internals.
(use-package helpful
  :defer t
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))

;; C-c followed by a control character belongs to major modes.  Put the
;; at-point helper in the user-reserved C-c h namespace instead.
(bind-key "d" #'helpful-at-point ltl/help-map)

(provide 'init-bindings)
;;; init-bindings.el ends here
