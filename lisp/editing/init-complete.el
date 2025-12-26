;;; init-complete.el --- Completion framework configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures a modern completion framework using:
;; - Company: In-buffer completion popup (works well in terminal & GUI)
;; - Vertico: Vertical completion UI for minibuffer
;; - Orderless: Flexible matching style
;; - Marginalia: Rich annotations in minibuffer
;; - Embark: Contextual actions on completion candidates
;; - Consult: Enhanced search and navigation commands
;;
;;; Code:

;;; ============================================================================
;;; Company Mode - Modern In-buffer Completion
;;; ============================================================================

(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :bind
  (:map company-mode-map
        ("C-M-i" . company-complete)
        ("M-/" . company-complete))
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-s" . company-filter-candidates)
        ("C-M-s" . company-search-candidates)
        ("<tab>" . company-complete-common-or-cycle)
        ("TAB" . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous)
        ("RET" . company-complete-selection)
        ("<return>" . company-complete-selection)
        ("C-w" . nil)  ; Don't interfere with kill-region
        ("C-h" . company-show-doc-buffer))
  :custom
  ;; Performance
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-tooltip-limit 14)
  
  ;; Behavior
  (company-selection-wrap-around t)
  (company-require-match nil)
  (company-show-quick-access t)
  (company-quick-access-keys '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
  (company-quick-access-modifier 'meta)  ; Use M-1, M-2 etc to select
  
  ;; Appearance
  (company-tooltip-align-annotations t)
  (company-tooltip-annotation-padding 1)
  (company-tooltip-margin 1)
  (company-format-margin-function #'company-text-icons-margin)
  
  ;; Backends
  (company-backends '((company-capf        ; completion-at-point-functions
                       company-files       ; file paths
                       company-keywords    ; programming keywords
                       company-yasnippet)  ; snippets
                      company-dabbrev-code ; code completions from buffer
                      company-dabbrev))    ; text completions from buffer
  
  ;; Dabbrev settings
  (company-dabbrev-other-buffers t)
  (company-dabbrev-ignore-case t)
  (company-dabbrev-downcase nil)
  
  :config
  ;; Better completion in comments and strings
  (setq company-dabbrev-char-regexp "\\sw\\|\\s_")
  
  ;; Make company work better with LSP
  (setq company-transformers '(delete-consecutive-dups
                               company-sort-by-occurrence
                               company-sort-prefer-same-case-prefix)))

;; Prescient - Intelligent sorting and filtering
(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (setq prescient-sort-full-matches-first t)
  (prescient-persist-mode 1))

(use-package company-prescient
  :after (company prescient)
  :config
  (company-prescient-mode 1))

;; Company-box - Better UI for GUI Emacs
(use-package company-box
  :if (display-graphic-p)
  :diminish
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-show-single-candidate t)
  (company-box-backends-colors nil)
  (company-box-doc-enable t)
  (company-box-doc-delay 0.3)
  (company-box-scrollbar nil))

;; Company-quickhelp - Documentation popup for terminal
(use-package company-quickhelp
  :unless (display-graphic-p)
  :hook (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-delay 0.3))

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

(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode))

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
