;;; init-bindings.el --- Configure key bindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;
;;; key bindings
;;;

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
               :prefix "C-c j"))

;; use evil when "C-c t e"
(use-package evil
  :bind
  (:map ltl/toggles-map
        ("e" . evil-mode)))

(defun ltl/unbind-all (fn)
  "Unbinds a function everywhere."
  (dolist (key (where-is-internal fn nil))
    (unbind-key key)))

;; C-c 2 to start mark region
(global-set-key (kbd "C-c 2")  #'set-mark-command)

;; avy is a GNU Emacs package for jumping to visible text using a
;; char-based decision tree
(use-package avy
  :bind
  (:map ltl/goto
        ("c" . #'avy-goto-char)
        ("j" . #'avy-goto-word-0)
        ("l" . #'avy-goto-line)))

;; change all prompts to y or n
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

;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings
;; recommended by the package author.
(use-package helpful
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))

(provide 'init-bindings)
;;; init-bindings.el ends here

