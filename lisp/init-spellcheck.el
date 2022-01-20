;;; init-spellcheck.el --- spellcheck
;;; Commentary:
;;; Code:

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(add-hook 'prog-mode #'flyspell-prog-mode)

(use-package flyspell-correct
  :after flyspell)

(use-package flyspell-correct-ivy
  :after flyspell-correct
  :bind (:map flyspell-mode-map
	      ("C-;" . flyspell-correct-wrapper)
	      ("M-C-;" . scimax-ivy-jump-to-typo)
	      ("s-M-;" . scimax-spellcheck/body)))

(provide 'init-spellcheck)
;;; init-spellcheck.el ends here
