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
;; Corfu keeps completion in a compact popup near point, which feels much
;; lighter than a full company-style framework and plays well with CAPF/Eglot.
(use-package corfu
  :defer t
  :custom
  ;; Offer candidates automatically so completion feels proactive instead
  ;; of requiring a manual trigger for every symbol.
  (corfu-auto t)
  ;; A short delay balances responsiveness with reduced distraction while typing.
  (corfu-auto-delay 0.1)
  ;; Start suggesting after a single character to help in languages with
  ;; long identifiers and keyword-heavy APIs.
  (corfu-auto-prefix 1)
  ;; Wrapping makes keyboard navigation continuous when browsing candidate lists.
  (corfu-cycle t)
  ;; Prefer the typed text first so accidental auto-selection does not
  ;; replace text the user intended to keep.
  (corfu-preselect 'prompt)
  ;; Show enough items to be useful without creating an oversized popup.
  (corfu-count 14)
  (corfu-max-width 80)
  ;; Do not auto-commit the exact match; this prevents completion UI from
  ;; feeling "sticky" when the typed text is already correct.
  (corfu-on-exact-match nil)
  ;; Quit at separators so file/path completion behaves naturally while
  ;; still reopening on the next component.
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
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              ;; Delay global activation until packages are ready so startup
              ;; stays clean and Corfu does not race Elpaca initialization.
              ;; Optional `require' keeps startup resilient if Corfu is not
              ;; installed yet, while still enabling it automatically once available.
              (when (require 'corfu nil t)
                (global-corfu-mode 1))))
  :config
  ;; `corfu-popupinfo' adds quick documentation for the current candidate,
  ;; which reduces trips to separate help buffers while completing.
  (require 'corfu-popupinfo nil t)
  (setq corfu-popupinfo-delay '(0.4 . 0.2))
  (when (fboundp 'corfu-popupinfo-mode)
    (corfu-popupinfo-mode 1)))

;; Nerd-icons integration for Corfu
;; Icons add a quick visual hint about candidate kind (function, variable,
;; file, etc.), which makes large completion lists easier to scan.
(use-package nerd-icons-corfu
  :defer t
  :init
  (with-eval-after-load 'corfu
    ;; Load icon support only after Corfu exists so the formatter hook
    ;; does not error on minimal installations.
    (when (require 'nerd-icons-corfu nil t)
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))))

;; Cape - Completion At Point Extensions
;; Cape enriches the standard completion-at-point pipeline instead of
;; replacing it, so built-in completion, Eglot, and Corfu can cooperate.
(use-package cape
  :defer t
  :init
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              ;; Optional loading means the rest of completion still works
              ;; even if Cape has not been installed yet.
              (require 'cape nil t)))
  ;; Add generic text/file/elisp fallbacks so completion remains useful
  ;; even when no language server is attached.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; Prescient - Intelligent sorting and filtering
;; Prescient learns from usage over time, so the commands and candidates
;; you pick most often bubble up faster in future sessions.
(use-package prescient
  :defer t
  :config
  ;; Combine multiple matching styles to keep filtering flexible without
  ;; giving up literal and prefix matches.
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  ;; Exact matches should still win when they exist; fuzzy should not bury
  ;; the obvious answer.
  (setq prescient-sort-full-matches-first t)
  (prescient-persist-mode 1))

;;; ============================================================================
;;; Yasnippet - Snippet expansion
;;; ============================================================================

;; `yasnippet' handles boilerplate expansion for common code patterns,
;; which cuts repetitive typing in programming and prose modes.
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  ;; Keep logs quiet unless something worth noticing happens.
  (setq yas-verbosity 1)
  ;; Wrapping the active region turns snippets into structured transforms
  ;; rather than insert-only templates.
  (setq yas-wrap-around-region t))

;; Pull in a large ready-made snippet catalog so the snippet framework is
;; useful immediately after install.
(use-package yasnippet-snippets
  :after yasnippet)

;;; ============================================================================
;;; Flycheck - Syntax checking
;;; ============================================================================

;; `flycheck' provides asynchronous syntax checking with clear fringe and
;; modeline feedback, which pairs well with Eglot and external linters.
(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode)
  :custom
  ;; Inherit the current load-path so local Emacs Lisp modules are linted
  ;; against the same environment they actually run in.
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'left-fringe)
  ;; Check on save and after short idle pauses to catch problems quickly
  ;; without firing on every keystroke.
  (flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (flycheck-idle-change-delay 0.5))

;;; ============================================================================
;;; Vertico Prescient
;;; ============================================================================

;; Reuse Prescient's ranking for Vertico so minibuffer completion learns
;; from history the same way Corfu-style completion does.
(use-package vertico-prescient
  :defer t
  :init
  (with-eval-after-load 'vertico
    ;; Only activate once both Vertico and the Prescient bridge are present.
    (when (require 'vertico-prescient nil t)
      (vertico-prescient-mode 1))))

;; Marginalia
;; Marginalia annotates minibuffer completions with some useful info.
;; These annotations add file modes, command categories, and other context
;; so similarly named candidates are easier to disambiguate.
(use-package marginalia
  :defer t
  :init
  (with-eval-after-load 'vertico
    ;; Marginalia is additive, so optional loading keeps minibuffer
    ;; completion functional even without the annotation package.
    (when (require 'marginalia nil t)
      (marginalia-mode 1))))


;; `orderless' allows typing space-separated components in any order,
;; which is dramatically faster for fuzzy minibuffer narrowing.
(use-package orderless
  :defer t
  :init
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              ;; Load Orderless after package init so minibuffer style setup
              ;; does not fail during first bootstrap.
              (require 'orderless nil t)))
  :custom
  ;; Use Orderless almost everywhere, but keep file completion friendlier
  ;; with partial completion for path segments like ~/s/d/file.
  (completion-styles '(orderless partial-completion basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult
;; Consult provides several enhanced functions for completing-read. It
;; fits nicely with Vertico.
;;
;; I generally remapped everything
;; obvious. consult-yank-from-kill-ring as a remapping of yank proved
;; a bit too disorienting.
;; `consult' upgrades many built-in navigation commands with preview,
;; history integration, and better minibuffer UX.
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
  ;; Show xref locations through Consult so "go to definition/references"
  ;; gets the same narrowing and preview behavior as the rest of minibuffer UX.
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref))

;; `consult-dir' makes it easy to jump between known directories from the
;; minibuffer, which reduces repetitive path typing.
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :package vertico
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; `embark' turns the current minibuffer candidate into an action hub:
;; open, export, grep, copy, etc., without leaving completion flow.
(use-package embark
  :defer t)

;; `embark-consult' gives live previews in Embark collect buffers so bulk
;; action lists remain navigable instead of becoming static dumps.
(use-package embark-consult
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Vertico is a little bit nicer version of the builtin
;; icomplete-vertical.
(use-package vertico
  :ensure (:files (:defaults "extensions/*"))
  :defer t
  :custom
  ;; Wrap candidate navigation to keep minibuffer movement fluid.
  (vertico-cycle t)
  ;; Case-insensitive buffer/file matching is friendlier in day-to-day use,
  ;; especially when exact capitalization is easy to forget.
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  ;; Keep default styles simple here; Orderless is configured above and
  ;; other packages may override categories when needed.
  (completion-styles '(basic substring partial-completion flex))
  :init
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              ;; Optional loading keeps startup robust when bootstrapping or
              ;; on machines where Vertico is intentionally absent.
              (when (require 'vertico nil t)
                (vertico-mode 1))))
  :bind ("M-R" . vertico-repeat)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("M-DEL" . vertico-directory-delete-word))
  :custom
  ;; Disable sorting for specific commands when preserving on-disk/project
  ;; order is more useful than generic ranking.
  (vertico-multiform-commands '((git-related-find-file (vertico-sort-function . nil))))
  :config
  ;; Load optional Vertico extensions piecemeal so the minibuffer gains
  ;; repeat/history/directory niceties without a separate package family.
  ;; These `require' calls are optional on purpose: each extension enhances
  ;; behavior if installed, but missing one should not break completion.
  (require 'vertico-indexed nil t)
  (require 'vertico-repeat nil t)
  (require 'vertico-directory nil t)
  (require 'vertico-multiform nil t)
  (when (fboundp 'vertico-repeat-save)
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))
  (when (fboundp 'vertico-directory-tidy)
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))
  (when (fboundp 'vertico-indexed-mode)
    (vertico-indexed-mode 1))
  (when (fboundp 'vertico-multiform-mode)
    (vertico-multiform-mode 1)))

;; auto-currects the workd you mistype on pressing space.
;; Enable abbrev expansion globally and save definitions silently so small
;; personal text shortcuts work everywhere without extra prompts.
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)


(provide 'init-complete)
