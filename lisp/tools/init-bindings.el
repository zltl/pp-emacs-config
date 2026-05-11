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

;;;
;;; key bindings
;;;


;; windows
;; On Windows, remap the physical modifier keys so Emacs gets a richer
;; set of shortcuts without fighting system-level defaults too much.
(setq w32-apps-modifier 'hyper)
(setq w32-lwindow-modifier 'super)
(setq w32-rwindow-modifier 'hyper)

;; use-package is built-in as of Emacs 29, but since we use :bind, we
;; need to load bind-key. If we forget, we get the error: Symbol's
;; value as variable is void: personal-keybindings.
(use-package bind-key
  :ensure nil
  :demand t
  ;; Define named prefix maps once so later modules can hang commands off
  ;; stable namespaces like C-c f / C-c t / C-c l.
  :bind
  (:prefix-map ltl/files-map
               :prefix "C-c f")
  :bind
  (:prefix-map ltl/toggles-map
               :prefix "C-c t")
  :bind
  (:prefix-map ltl/goto
               :prefix "C-c j")
  :bind
  (:prefix-map ltl/smartparen
               :prefix "C-c p")
  :bind
  (:prefix-map ltl/multicursor
               :prefix "C-c m")
  :bind
  (:prefix-map ltl/lsp-map
               :prefix "C-c l"))

;; use evil when "C-c t e"
;; Keep Evil opt-in instead of always on: this preserves vanilla Emacs
;; editing by default while still making modal editing one toggle away.
(use-package evil
  :bind
  (:map ltl/toggles-map
        ("e" . evil-mode)))

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
  (:map ltl/goto
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
    "C-c t" "toggles"
    "C-c j" "goto"
    "C-c p" "smartparens"
    "C-c m" "multicursor"
    "C-c l" "lsp/eglot"))

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
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))

(provide 'init-bindings)
;;; init-bindings.el ends here
