

(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :hook (corfu-mode . corfu-history-mode)
  :custom
  (corfu-auto t) ; Enable auto completion
  (corfu-cycle t) ; Allows cycling through candidates
  (corfu-min-width 25)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-quit-no-match 'separator)
  :config
  (unless (bound-and-true-p savehist-mode)
    (savehist-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package prescient)
(use-package corfu-prescient
  :hook (corfu-mode . corfu-prescient-mode))
(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode))
(use-package vertico-prescient
  :hook (vertico-mode . vertico-prescient-mode))
(use-package cape)
(use-package popon
  :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))

;; corfu cannot used in terminal
;; try corfu-terminal
(use-package corfu-terminal
  :straight (corfu-terminal
             :type git
             :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :hook (corfu-mode . corfu-terminal-mode))

(use-package nerd-icons-corfu)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
;; Optionally:
(setq nerd-icons-corfu-mapping
      '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
        (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
        ;; ...
        (t :style "cod" :icon "code" :face font-lock-warning-face)))
        ;; Remember to add an entry for `t', the library uses that as default.
;; The Custom interface is also supported for tuning the variable above.



(use-package yasnippet
  :diminish
  :config
  (yas-global-mode 1))
;; flycheck
(use-package flycheck
  :diminish
  :init
  (global-flycheck-mode))

;; Marginalia
;; Marginalia annotates minibuffer completions with some useful info.
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :straight t
  :demand t
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
  :straight t
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

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(provide 'init-complete)
