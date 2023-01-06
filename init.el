;;; init.el --- Load the full configuration
;;; Commentary:
;;; Code:

;; encoding
(prefer-coding-system 'utf-8)


;; simple face
(menu-bar-mode -1)
;; not backup
(setq make-backup-files nil)



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
	'swiper
	))

(dolist (e *use-package-list*)
  (straight-use-package e))


(require 'org)
(add-hook 'org-mode-hook
	  (lambda ()
	    (org-indent-mode)
	    (org-bullets-mode 1)
	    (setq org-agenda-files
		  (file-expand-wildcards "~/TODO/*.org"))))


(load-theme 'spacemacs-dark t)

(require 'smartparens-config)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (smartparens-mode)))


(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)
(which-key-mode)
(yas-global-mode)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'go-mode-hook 'lsp)
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



(provide 'init)
;;; init.el ends here

