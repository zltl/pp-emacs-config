;;; package -- init-copilot -- setup copilot

;;; Commentary:

;;; Code:

;; copilot
;; I think Copilot’s training was unethical, and I’m skeptical of its
;; utility, but I need to get some experience with it.

;; always in copilot-disable-predicates turns off automatic
;; completion. We can still reach it from M-`, which is chosen to be
;; close to M-TAB and bound to a menubar command I don’t ever use.

;; TODO: use M-x copilot-login
(when (executable-find "node")
  (use-package copilot
    :diminish
    :straight (:host github :repo "copilot-emacs/copilot.el")
    :custom
    (copilot-disable-predicates '(always))
    :hook
    (prog-mode . copilot-mode)
    :bind
    ("M-`" . copilot-complete)
    :bind
    (:map ltl/toggles-map
          ("`" . copilot-mode))
    :bind
    (:map copilot-completion-map
          ("C-g" .  #'copilot-clear-overlay)
          ("M-p" . #'copilot-previous-completion)
          ("M-n" . #'copilot-next-completion)
          ("TAB" . #'copilot-accept-completion)
          ("M-f" . #'copilot-accept-completion-by-word)
          ("M-<return>" . #'copilot-accept-completion-by-line))
    :config
    (and (not (copilot-installed-version))
         (copilot-install-server))))

(provide 'init-copilot)

;;; init-copilot.el ends here

