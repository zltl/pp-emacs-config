;;; init-completion.el --- complection config

;;; Code:

(use-package counsel)
(diminish 'ivy-mode)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode-on)
  (yas-minor-mode . (lambda () (diminish 'yas-minor-mode))))
(use-package yasnippet-snippets)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  "If company-mode/enable-yas, put company-yasnippet into BACKEND."
  (if (or (not company-mode/enable-yas)
	  (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(use-package company
  :ensure t
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-show-quick-access t
        company-minimum-prefix-length 1)
  (setq company-backends
	(mapcar #'company-mode/backend-with-yas company-backends)))

(provide 'init-completion)
;;; init-completion.el ends here
