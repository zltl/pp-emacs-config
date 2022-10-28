;;; init.el --- Load the full configuration
;;; Commentary:
;;; Code:

(defconst emacs-start-time (current-time))

(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; add ./lisp folder into load-path, so we can split the configure files
;; into this directory.
;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'eldoc)


;; encoding
(prefer-coding-system 'utf-8)


;; install straight.el
(let* ((reposupd (expand-file-name "straight/repos" user-emacs-directory))
       (ob (get-buffer "*Messages*"))
       (repodir (expand-file-name "straight.el" reposupd))
       (bootstrap-file (expand-file-name "bootstrap.el" repodir))
       (clone-cmd (format "git clone git@github.com:radian-software/straight.el.git %s" repodir)))
  (unless (file-exists-p reposupd)
    (make-directory reposupd t))
  (unless (file-exists-p bootstrap-file)
    (message "executing %s ..." clone-cmd)
    (shell-command clone-cmd ob ob))
  (load bootstrap-file))


;; switch window fast
(straight-use-package 'ace-window)
(require 'ace-window)
(global-set-key (kbd "C-x o") #'ace-window)


;; C-q C-l to insert line
(straight-use-package
 '(page-break-lines :type git :host github :repo "purcell/page-break-lines"))
(global-page-break-lines-mode)


;; Help keeping ~/.emacs.d clean
(straight-use-package 'no-littering)
(require 'no-littering)
;; no littering suggested settings
(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
;; auto-save setting
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
;; customizations
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))


;; theme
(straight-use-package '(spacemacs-theme :repo "nashamri/spacemacs-theme"))
(load-theme 'spacemacs-dark t)

;; modeline
(straight-use-package 'doom-modeline)
(doom-modeline-mode t)
(setq doom-modeline-project-detection 'project)
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el"
;;
;; If you are expereicing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'relative-from-project)


;; displays available keybindings in popup
(straight-use-package '(which-key :repo "justbur/emacs-which-key"))
(require 'which-key)
(which-key-mode)


;; parens

(straight-use-package '(smartparens :repo "Fuco1/smartparens"))
(require 'smartparens-config)
(sp-use-smartparens-bindings)
(show-smartparens-global-mode t)
(add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)

(straight-use-package 'paren)
(require 'paren)
(setq show-paren-delay 0.1
      show-paren-when-point-in-periphery t)

(straight-use-package 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;; do not request confirm before visiting a new file.
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward)

;; no beeping or blinking
(setq ring-bell-function #'ignore
      visible-bell nil)

;; disable menu bar, tool-bar
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq inhibit-startup-screen t) ;; stop showing startup screen

;; oh my freaking god, just take my damn answer
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      ;; version-control t     ; number each backup file
      ;; backup-by-copying t   ; instead of renaming current file (clobbers links)
      ;; delete-old-versions t ; clean up after itself
      ;; kept-old-versions 5
      ;; kept-new-versions 5
      )

;; (setq auto-save-default t)
;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
;; (setq auto-save-default t
;;       ;; Don't auto-disable auto-save after deleting big chunks. This defeats
;;       ;; the purpose of a failsafe. This adds the risk of losing the data we
;;       ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
;;       auto-save-include-big-deletions t
;; 	  auto-save-file-name-transforms
;;       (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
;;                   ;; Prefix tramp autosaves to prevent conflicts with local ones
;;                   (concat auto-save-list-file-prefix "tramp-\\2") t)
;;             (list ".*" auto-save-list-file-prefix t)))

;; Show colume number
(setq-default column-number-mode t)
(setq-default word-wrap t)
;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)

(visual-line-mode)

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant (and no big deal if not).
(setq require-final-newline t)

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-do-not-save-duplicates t)

;; Keeping buffers automatically up-to-date.
(require 'autorevert)
(global-auto-revert-mode 1)
(setq auto-revert-verbose t
      auto-revert-use-notify nil
      auto-revert-stop-on-user-input nil)


;; treemacs
(straight-use-package 'neotree)
(require 'neotree-autoloads)
(straight-use-package 'treemacs)
(require 'treemacs-autoloads)
(global-set-key (kbd "C-c t") #'treemacs-select-window)

;; ace-window ignore treemacs default, use 'C-x o' move windows including treemacs.
(add-hook 'treemacs-mode-hook
	  (lambda()
	    (setq aw-ignored-buffers (delete 'treemacs-mode aw-ignored-buffers))))

;; (straight-use-package 'lsp-treemacs)
;; (lsp-treemacs-sync-mode 1)


;; hightlight todo
(straight-use-package 'hl-todo)
(add-hook 'prog-mode-hook #'hl-todo-mode)


;; projectile

(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)



;; recentf
(require 'recentf)
(defun my/recentf-save-list-silence ()
  "Save recentf."
  (interactive)
  (let ((mesage-log-max nil))
    (if (fboundp 'shut-up)
        (shut-up (recentf-save-list))
      (recentf-save-list)))
  (message ""))

(defun my/recentf-cleanup-silence()
  "Clean recentf."
  (interactive)
  (let ((message-log-max nil))
    (if (fboundp 'shut-up)
        (shut-up (recentf-cleanup))
      (recentf-cleanup)))
  (message ""))

(recentf-mode 1)
(global-set-key (kbd "C-x C-b") #'recentf-open-files)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 100)
(setq recentf-auto-cleanup 'never)
(add-hook 'focus-out-hook #'my/recentf-save-list-silence t nil)
(add-hook 'focus-out-hook #'my/recentf-cleanup-silence t nil)
(with-eval-after-load 'no-littering
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude no-littering-var-directory))
(add-to-list 'recentf-exclude (expand-file-name "elpa" user-emacs-directory))
(add-to-list 'recentf-exclude (expand-file-name "cache" user-emacs-directory))


;; more contextual information of help
(straight-use-package 'helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)


;; completion/search

;; ivy
(straight-use-package 'counsel)
(straight-use-package '(ivy :repo "abo-abo/swiper"))
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)

(define-key prog-mode-map (kbd "M-,") 'pop-global-mark)

;; ag
(straight-use-package 'ag)

;; yas
(straight-use-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)

;; company
(straight-use-package 'company)
(global-company-mode t)
(setq company-idle-delay 0.2
      company-tooltip-align-annotations t
      company-tooltip-limit 20
      company-show-quick-access t
      company-minimum-prefix-length 1)


;; flycheck

(straight-use-package 'flycheck)
(global-flycheck-mode)


;; git
(straight-use-package 'magit)

;; org
(require 'org)

(defun chinese/post-init-org ()
  "Remove space between chinese workds when exporting."
  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents))))

(defun my/org-mode-setup ()
    "Setup orgmode."
    (org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (chinese/post-init-org))
(require 'org-tempo)
  (setq org-ellipsis " ▼"
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-states-order-reversed t
        org-log-into-drawer 'show2levels
        org-startup-folded t)
  (setq org-agenda-files
        (file-expand-wildcards "~/TODO/*.org"))

(straight-use-package 'org-bullets)
(add-hook 'org-mode-hook #'org-bullets-mode)
(add-hook 'org-mode-hook #'my/org-mode-setup)


;; lsp
(straight-use-package 'lsp-mode)
(setq-default lsp-keymap-prefix "C-c C-l")
(require 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-ivy)


;; use dump jump on shell script mode
(straight-use-package 'dumb-jump)

(add-hook 'shell-script-mode #'dump-jump-mode)

(cl-defun lsp-find-definition-or-dumb-jump ()
  "Fallback dump jump when using lsp."
  (interactive)
  (let ((loc (lsp-request "textDocument/definition"
                          (lsp--text-document-position-params))))
    (if (seq-empty-p loc)
        (dumb-jump-go) ;; todo: this is technically deprecated
      (lsp-show-xrefs (lsp--locations-to-xref-items loc) nil nil))))



;; golang

(straight-use-package 'go-mode)
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-hook 'go-mode-hook #'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  "Fomat and import when save."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(straight-use-package
 '(go-tag :type git :host github :repo "brantou/emacs-go-tag"))
(require 'go-tag)
(setq go-tag-args (list "-transform" "snakecase"))


;; c/c++

(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Auto-completion for C/C++ headers using Company
(straight-use-package 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

(straight-use-package 'cpp-auto-include)

(straight-use-package 'disaster)

(straight-use-package 'gdb-mi)
(setq gdb-many-windows t
      gdb-show-main t)

;; cmake
(straight-use-package 'cmake-mode)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; c++20
(straight-use-package 'modern-cpp-font-lock)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; google style, but with 4 space indent.
(straight-use-package 'google-c-style)
(require 'google-c-style)
(defun google-set-c-style-with-4-indent ()
  "Set current buffer to google style, but with 4 space indent."
  (interactive)
  (setq c-tab-always-indent t)
  ;; linux source code use linux style
  (if (string-match "\\(linux|Linux\\)" (file-name-directory buffer-file-name))
      ;; if case
      (setq c-default-style "linux")

    ;; else case
    (progn
      (google-set-c-style)
      (setq tab-width 8
            c-indent-tabs-mode t
            c-indent-level 4
            c-basic-offset 4))))
(add-hook 'c-mode-common-hook 'google-set-c-style-with-4-indent)



;; python
(straight-use-package 'elpy)
(add-hook 'python-mode #'elpy-enable)
(setq python-indent-offset 4)
(add-hook 'python-mode-hook (lambda () (setq python-indent 4)))


;; rust
(straight-use-package 'rustic)
(add-hook 'rustic-mode #'lsp-deferred)


;; protobuf
(straight-use-package 'protobuf-mode)
(require 'protobuf-mode)


;; yaml csv json

(straight-use-package 'yaml-mode)
(require 'yaml-mode)
(straight-use-package 'csv-mode)
(require 'csv-mode)
(straight-use-package 'json-mode)
(straight-use-package 'json-navigator)
(straight-use-package 'json-reformat)
(straight-use-package 'json-snatcher)
(require 'json-mode)

;; web

(straight-use-package 'prettier-js)
(straight-use-package 'web-beautify)
(straight-use-package 'company-web)
(straight-use-package 'web-mode)
(require 'web-mode)
(straight-use-package 'js2-mode)
(setq js2-basic-offset 2)
(setq js-indent-level 2)


;; lua
(straight-use-package 'lua-mode)
(require 'lua-mode)


;; undotree
(straight-use-package 'undo-tree)
(require 'undo-tree-autoloads)
;; comment these when calling straight-pull-all
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)


(straight-use-package '(slime :repo "slime/slime"))
(require 'slime)

;; TODO: in sbcl (ql:quickload "quicklisp-slime-helper")

(let ((slime-file "~/.quicklisp/slime-helper.el"))
  (when (file-exists-p slime-file)
    (load (expand-file-name slime-file))))
(setq inferior-lisp-program "sbcl")


(provide 'init)
;;; init.el ends here

