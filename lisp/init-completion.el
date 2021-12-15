;;; init-completion.el --- complection config

;;; Code:

(use-package counsel)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode-on)
  (yas-minor-mode . (lambda () (diminish 'yas-minor-mode))))
(use-package yasnippet-snippets)

(use-package ivy :demand
  :diminish nil
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))
(use-package company
  :ensure t
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-show-quick-access t
        company-minimum-prefix-length 1))

(provide 'init-completion)
;;; init-completion.el ends here
