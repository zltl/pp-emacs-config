;;; init-minuet.el --- Inline AI completion via minuet -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Ghost-text completion using minuet.
;; Default provider is openai-fim-compatible so cheap/self-hosted endpoints
;; (DeepSeek, OpenRouter, local OpenAI-compatible servers) can be used.
;; Suggestions are on-demand by default to control cost.
;;
;;; Code:

(use-package minuet
  :defer t
  :custom
  ;; Prefer FIM-compatible endpoints; override model/endpoint in auth/env setup.
  (minuet-provider 'openai-fim-compatible)
  :bind
  (("M-`" . minuet-complete-with-minibuffer)
   :map minuet-active-mode-map
   ("M-A" . minuet-accept-suggestion)
   ("M-a" . minuet-accept-suggestion-line)
   ("M-w" . minuet-accept-suggestion-word)
   ("M-e" . minuet-dismiss-suggestion))
  :bind
  (:map ltl/tools-map
        ("m" . minuet-auto-suggestion-mode))
  :config
  ;; Reduce automatic request volume if the user enables auto suggestions.
  (setq minuet-auto-suggestion-debounce-delay 0.6
        minuet-auto-suggestion-throttle-delay 1.5)
  ;; Allow overriding endpoint/model via env for remote pods without editing Lisp.
  (when-let* ((endpoint (getenv "MINUET_ENDPOINT")))
    (plist-put minuet-openai-fim-compatible-options :end-point endpoint))
  (when-let* ((model (getenv "MINUET_MODEL")))
    (plist-put minuet-openai-fim-compatible-options :model model))
  (when-let* ((key-env (getenv "MINUET_API_KEY_ENV")))
    (plist-put minuet-openai-fim-compatible-options :api-key key-env)))

(provide 'init-minuet)
;;; init-minuet.el ends here
