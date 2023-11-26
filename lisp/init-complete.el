

(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  ;; You may want to play with delay/prefix/styles to suit your preferences.
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (completion-styles '(orderless)))
(use-package prescient)
(use-package corfu-prescient
  :hook (corfu-mode . corfu-prescient-mode))
(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode))
(use-package vertico-prescient
  :hook (vertico-mode . vertico-prescient-mode))
(use-package cape)
(use-package popon
  :quelpa (popon :fetcher git
                 :url "https://codeberg.org/akib/emacs-popon.git"))

;; corfu cannot used in terminal
;; try corfu-terminal
(use-package corfu-terminal
  :quelpa (corfu-terminal
           :fetcher git
           :url "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))
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

;; window selection with ace-window
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :bind ("M-0" . treemacs-select-window))

;; Vertico is a little bit nicer version of the builtin
;; icomplete-vertical.
(use-package vertico
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


(provide 'init-complete)
