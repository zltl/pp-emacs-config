;;; init.el --- Load the full configuration
;;; Commentary:
;;; Code:

;; encoding
(prefer-coding-system 'utf-8)


;; simple face
(menu-bar-mode -1)

(if window-system
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

;; font size
(set-face-attribute 'default nil :height 140)
;; not backup
(setq make-backup-files nil)
;; show line and column on mode-line
(setf line-number-mode t
      column-number-mode t)
;; load file when other program modify files that opening.
(global-auto-revert-mode)


;; straignt.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; install all package we need here
(defvar *use-package-list*
  (list 'lsp-mode
        '(lsp-ui :repo "emacs-lsp/lsp-ui" :host github)
        'lsp-treemacs
        '(copilot :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
        '(tsi :type git :host github :repo "orzechowskid/tsi.el")
        '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
        'web-mode
        'highlight-thing
        'helm-lsp
        'dashboard
        'go-add-tags
        'projectile
        'hydra
        'flycheck
        'company
        'avy
        'which-key
        'helm-xref
        'dap-mode
        'yasnippet
        'go-mode
        'ag
        'helm-ag
        'spacemacs-theme
        'treemacs
        'lsp-treemacs
        'magit
        'org-bullets
        'markdown-mode
        'markdown-mode+
        'json-mode
        'sly
        'smartparens
        'rainbow-delimiters
        'goto-line-preview
        'swiper
        'page-break-lines
        'undo-tree
        'modern-cpp-font-lock
        'ace-window
        'clang-format
        'rust-mode
        'typescript-mode
        'tide
        'iedit
        'ace-popup-menu
        'selectrum
        'selectrum-prescient
        'orderless
        'nyan-mode
        'hungry-delete
        'spaceline
        'helpful))
(dolist (e *use-package-list*)
  (straight-use-package e))


(selectrum-mode +1)
;; to make sorting and filtering more intelligent
(selectrum-prescient-mode +1)
;; to save your command history on disk, so the sorting gets more
;; intelligent over time
(prescient-persist-mode +1)

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))


(dashboard-setup-startup-hook)
(setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
(setq dashboard-set-navigator t)
(setq dashboard-set-init-info t)
(setq dashboard-set-footer nil)
(setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
(add-to-list 'dashboard-items '(agenda) t)
(setq dashboard-week-agenda t)
(setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
(ace-popup-menu-mode 1)

(require 'spaceline-config)
(spaceline-spacemacs-theme)
(nyan-mode)
(add-hook 'prog-mode-hook 'highlight-thing-mode)
(hungry-delete-mode)


;; orgmode config
(require 'org)
;; fold content when open
(setf org-startup-folded 'show2levels)
(add-hook 'org-mode-hook
          (lambda ()
            ;; indent by levels
            (org-indent-mode)
            (setf org-src-preserve-indentation nil
                  org-edit-src-content-indentation 0)

            ;; make title look better
            (org-bullets-mode 1)
            ;; add all todo org file to agenda, for me, all todo files are
            ;; list in ~/TODO/ folder, and sync by a net drive service.
            (setq org-agenda-files
                  (file-expand-wildcards "~/TODO/*.org"))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))


;; just take my answer!
(defalias 'yes-or-no-p 'y-or-n-p)

;; looks good enough
(load-theme 'spacemacs-dark t)
(setq mode-line-format
      (list
       '(:eval (list (nyan-create)))))

;; Visualize tabs as a pipe character - "|"
;; This will also show trailing characters as they are useful to spot.
;; Use brighter color for parens.
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#e91e63"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#2196F3"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#EF6C00"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#B388FF"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#76ff03"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#26A69A"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#FFCDD2"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#795548"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#DCE775"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#FFFFFF" :background "#EF6C00"))))
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;; undotree C-x u
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; hungry delete
(setq backward-delete-char-untabify-method 'hungry)

;; better help pages.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; recentf
(global-set-key (kbd "C-x b") #'recentf-open-files)

(global-set-key (kbd "C-c g") #'avy-goto-char-2)

;; enable mouse
(xterm-mouse-mode)


;; I prefer smartparens-strict-mode, but always forget what sp-xxx
;; command should I use, so be humble.
(require 'smartparens-config)
(add-hook 'prog-mode-hook
          (lambda ()
            (smartparens-mode)))

;; prefer helm
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)
;; completion for shortcut
(which-key-mode)
;; yasnippets, just for lsp completion.
;; looks wired if not enable it when lsp complete your code.
(yas-global-mode)

;; A good search, replace C-s
(global-set-key "\C-s" #'swiper)

(global-set-key [remap goto-line] 'goto-line-preview)


;; M-o (Am Oh) to switch to other window.
;; M-0 (Am Zero) to open, or switch to treemacs window.
(global-set-key (kbd "M-o") #'ace-window)
(global-set-key (kbd "M-0") #'treemacs-select-window)

;; web
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(defun web-mode-config ()
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        typescript-indent-level 2
        web-mode-enable-current-element-highlight t)
  (setf (alist-get 'web-mode lsp--formatting-indent-alist)
        'web-mode-code-indent-offset)
  (m/indent-space))
(add-hook 'web-mode-hook #'web-mode-config)

(setq-default typescript-indent-level 2)
(setq-default js-indent-level 2)
(setq-default js2-basic-offset 2)
(setq-default js3-indent-level 2)
(setq-default typescript-ts-mode-indent-offset 2)
(defun lsp-ts-install-save-hooks ()
  (add-hook 'before-save-hook #'tide-format-before-save)
  (tide-hl-identifier-mode)
  (tide-setup)
  (tide-mode)
  (tide-hl-identifier-mode)
  (web-mode-config)
  (setq typescript-indent-level 2)
  (m/indent-space)
  (subword-mode))
(add-hook 'typescript-mode-hook #'lsp-ts-install-save-hooks)


;; enable lsp for C/C++/go/rust
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
(add-hook 'go-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'web-mode-hook #'lsp)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (custom-set-variables
   '(go-add-tags-style 'lower-camel-case)))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; The Rust style guide recommends spaces rather than tabs for indentation
(defun turn-off-indent-tabs-mode ()
  (setq indent-tabs-mode nil))
(add-hook 'rust-mode-hook #'turn-off-indent-tabs-mode)

(with-eval-after-load 'c-mode
  (lambda () (require 'dap-cpptools)))
(with-eval-after-load 'c++-mode
  (lambda () (require 'dap-cpptools)))

;; use c++ in .h default.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; not gc too much.
;; And other value for lsp.
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.5)  ;; clangd is fast

;; enable which-key
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook
            (lambda ()
              (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
              (let ((lsp-keymap-prefix "C-c l"))
                (lsp-enable-which-key-integration)))))
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\target\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\deps\\'")
  (setq lsp-log-io nil)
  )



;; copilot
(add-hook 'prog-mode-hook 'copilot-mode)
(with-eval-after-load 'company
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0) ;; default is 0.2
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(add-hook 'copilot-mode-hook
          (lambda ()
            (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
            (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)))


;; lisp mode config

;; And sh mode
(add-hook 'sh-mode-hook #'turn-off-indent-tabs-mode)
(defun my-lisp-hook ()
  "common hook of elisp and common lisp."
  ;; Make ^L (C-q-l) looks as horizontal line on lisp
  (page-break-lines-mode)
  (turn-off-indent-tabs-mode))
(add-hook 'emacs-lisp-mode-hook #'my-lisp-hook)
(add-hook 'lisp-mode-hook #'my-lisp-hook)


;; For convenient to set tab-width.
;; I works on multile project that follow different code style.
;; Sad for that.
(defun m/tab-width (w)
  "Set tab width."
  (interactive "nNew Tab Width: ")
  (setf tab-width w))
(defun m/indent-space ()
  "Use tab indent."
  (interactive)
  (setf indent-tabs-mode nil
        tab-always-indent t))
(defun m/indent-tab ()
  "Use tab indent."
  (interactive)
  (setq indent-tabs-mode t))


;; latex
(setq-default TeX-engine 'xelatex)
(setq-default Tex-PDF-mode t)



(provide 'init)
