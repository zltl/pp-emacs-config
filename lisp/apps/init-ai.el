;;; init-ai.el --- AI/LLM integration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; AI assistant integration using gptel.
;; Supports multiple backends: OpenAI, Claude, Ollama, etc.
;; Configure API keys via auth-source (~/.authinfo.gpg).
;;
;;; Code:

;; `gptel' is a general-purpose LLM client for Emacs, useful when you want
;; chat, drafting, or prompt workflows directly in buffers.
(use-package gptel
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :custom
  ;; Default to Org so AI sessions support headings, lists, and code blocks.
  (gptel-default-mode 'org-mode)
  :config
  ;; Key bindings for gptel
  (global-set-key (kbd "C-c a a") #'gptel)
  (global-set-key (kbd "C-c a s") #'gptel-send)
  (global-set-key (kbd "C-c a m") #'gptel-menu))

(provide 'init-ai)
;;; init-ai.el ends here
