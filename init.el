;;; init.el --- Load the full configuration
;;; Commentary:
;;; Code:

;; encoding
(prefer-coding-system 'utf-8)


;; simple face
(menu-bar-mode -1)
;; not backup
(setq make-backup-files nil)
(setf line-number-mode t
      column-number-mode t)


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


(defvar *use-package-list*
  (list 'lsp-mode
	'lsp-ui
	'lsp-treemacs
	'helm-lsp
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
	'spacemacs-theme
	'treemacs
	'magit
	'org-bullets
	'markdown-mode
	'json-mode
	'sly
	'smartparens
	'rainbow-delimiters
	'swiper
	'page-break-lines
	'undo-tree
	'helpful))

(dolist (e *use-package-list*)
  (straight-use-package e))


(require 'org)
(setf org-startup-folded 'show2levels)
(add-hook 'org-mode-hook
	  (lambda ()
	    (org-indent-mode)
	    (org-bullets-mode 1)
	    (setq org-agenda-files
		  (file-expand-wildcards "~/TODO/*.org"))))


(load-theme 'spacemacs-dark t)

;; WARNING: This will change your life
;; (OPTIONAL) Visualize tabs as a pipe character - "|"
;; This will also show trailing characters as they are useful to spot.
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(global-undo-tree-mode)

;; helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(require 'smartparens-config)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (smartparens-mode)))

(setq backward-delete-char-untabify-method 'hungry)

(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)
(which-key-mode)
(yas-global-mode)
;; replace C-s
(global-set-key "\C-s" 'swiper)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'go-mode-hook #'lsp)

(defun my-lisp-hook ()
  "common hook of elisp and common lisp."
  (page-break-lines-mode))
(add-hook 'emacs-lisp-mode-hook #'my-lisp-hook)
(add-hook 'lisp-mode-hook #'my-lisp-hook)

(with-eval-after-load 'c-mode
  (lambda () (require 'dap-cpptools)))
(with-eval-after-load 'c++-mode
  (lambda () (require 'dap-cpptools)))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))


(defun m/tab-width (w)
  "Set tab width."
  (interactive "nNew Tab Width: ")
  (setf tab-width w))




(provide 'init)
;;; init.el ends here

