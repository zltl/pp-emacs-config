;;; init-copilot.el --- GitHub Copilot and AI assistance -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures AI-powered coding assistance:
;; - GitHub Copilot: AI pair programmer
;; - ChatGPT integration: AI chat in Emacs
;;
;; Note: Requires Node.js and proper authentication.
;; Can be disabled via `ltl/enable-copilot` variable.
;;
;;; Code:

;; copilot
;; I think Copilot’s training was unethical, and I’m skeptical of its
;; utility, but I need to get some experience with it.

;; always in copilot-disable-predicates turns off automatic
;; completion. We can still reach it from M-`, which is chosen to be
;; close to M-TAB and bound to a menubar command I don’t ever use.

;; TODO: use M-x copilot-login
(when (executable-find "node")
  ;; `track-changes' is used by Copilot's overlay/update machinery; load it
  ;; explicitly so the dependency is visible to future maintainers.
  (use-package track-changes
    :ensure (:host github :repo "emacs-straight/track-changes"
             :files ("*.el")))

  ;; `copilot' provides inline AI completions. Guarding it with a Node
  ;; check avoids enabling a feature whose backend cannot start.
  (use-package copilot
    :diminish
    :ensure (:host github :repo "copilot-emacs/copilot.el")
    :custom
    ;; Disable automatic suggestions by default so ghost text does not
    ;; compete with Corfu/normal editing until explicitly requested.
    (copilot-disable-predicates '(always))
    :hook
    (prog-mode . copilot-mode)
    :bind
    ("M-`" . copilot-complete)
    :bind
    (:map ltl/toggles-map
          ("`" . #'copilot-mode))
    :bind
    (:map copilot-completion-map
          ("C-g" .  #'copilot-clear-overlay)
          ("M-p" . #'copilot-previous-completion)
          ("M-n" . #'copilot-next-completion)
          ("TAB" . #'copilot-accept-completion)
          ("M-f" . #'copilot-accept-completion-by-word)
          ("M-<return>" . #'copilot-accept-completion-by-line))
    :config
    ;; Install the Copilot server automatically on first use so setup is
    ;; mostly self-service once authentication is complete.
    (when (not (copilot-installed-version))
      (let ((display-buffer-alist (cons '("^\\*copilot-install-server\\*" display-buffer-no-window (allow-no-window . t)) display-buffer-alist)))
        (copilot-install-server)))))


;; ChatGPT Shell - AI chat integration (optional)
;; Note: Requires shell-maker v0.77.1+
;; Uncomment to enable after ensuring dependencies are up to date:
;;
;; (use-package shell-maker
;;   :defer t
;;   :ensure (:host github :repo "xenodium/shell-maker" :files ("shell-maker*.el")))
;; 
;; (use-package chatgpt-shell
;;   :defer t
;;   :ensure (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el"))
;;   :after shell-maker
;;   :commands (chatgpt-shell chatgpt-shell-mode))
;; 
;; (use-package copilot-chat
;;   :defer t
;;   :ensure (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
;;   :after (request org markdown-mode shell-maker)
;;   :commands (copilot-chat-display)
;;   :bind
;;   (:map ltl/toggles-map
;;         ("c" . #'copilot-chat-display)))

(provide 'init-copilot)

;;; init-copilot.el ends here
