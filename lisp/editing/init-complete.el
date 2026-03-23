;;; init-complete.el --- Completion framework configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures a modern completion framework using:
;; - Corfu: In-buffer completion popup (lightweight, by minad)
;; - Cape: Completion-at-point extensions
;; - Vertico: Vertical completion UI for minibuffer
;; - Orderless: Flexible matching style
;; - Marginalia: Rich annotations in minibuffer
;; - Embark: Contextual actions on completion candidates
;; - Consult: Enhanced search and navigation commands
;;
;;; Code:

;;; ============================================================================
;;; In-buffer Completion (Corfu + Cape)
;;; ============================================================================

;; Corfu - Lightweight in-buffer completion popup
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-count 14)
  (corfu-max-width 80)
  (corfu-on-exact-match nil)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<tab>" . corfu-next)
        ("TAB" . corfu-next)
        ("<backtab>" . corfu-previous)
        ("RET" . corfu-insert)
        ("<return>" . corfu-insert)
        ("C-g" . corfu-quit)
        ("M-d" . corfu-popupinfo-toggle)
        ("M-p" . corfu-popupinfo-scroll-down)
        ("M-n" . corfu-popupinfo-scroll-up))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.4 . 0.2)))

;; Nerd-icons integration for Corfu
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Cape - Completion At Point Extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; Prescient - Intelligent sorting and filtering
(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (setq prescient-sort-full-matches-first t)
  (prescient-persist-mode 1))

;;; ============================================================================
;;; Yasnippet - Snippet expansion
;;; ============================================================================

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t))

(use-package yasnippet-snippets
  :after yasnippet)

;;; ============================================================================
;;; Flycheck - Syntax checking
;;; ============================================================================

(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'left-fringe)
  (flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (flycheck-idle-change-delay 0.5))

;;; ============================================================================
;;; Vertico Prescient
;;; ============================================================================

(use-package vertico-prescient
  :after vertico
  :config
  (vertico-prescient-mode 1))

;; Marginalia
;; Marginalia annotates minibuffer completions with some useful info.
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))


(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult
;; Consult provides several enhanced functions for completing-read. It
;; fits nicely with Vertico.
;;
;; I generally remapped everything
;; obvious. consult-yank-from-kill-ring as a remapping of yank proved
;; a bit too disorienting.
(use-package consult
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

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :package vertico
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark)

(use-package embark-consult
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Vertico is a little bit nicer version of the builtin
;; icomplete-vertical.
(use-package vertico
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (require 'vertico-indexed)
  (vertico-indexed-mode)  (vertico-mode)
  (require 'vertico-repeat)
  (require 'vertico-directory)
  (require 'vertico-multiform)
  :hook
  (minibuffer-setup . vertico-repeat-save)
  :bind ("M-R" . vertico-repeat)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-multiform-commands '((git-related-find-file (vertico-sort-function . nil))))
  :config
  (vertico-multiform-mode))

;; auto-currects the workd you mistype on pressing space.
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)


(provide 'init-complete)
