;;; init-text.el --- Text editing and manipulation -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures text editing enhancements:
;; - DWIM (Do What I Mean) case operations
;; - Title case conversion
;; - Multiple cursors
;; - Text manipulation utilities
;;
;;; Code:

;;;
;;; Text
;;;

;; DWIM case
;; These do-what-I-mean bindings are newer than the classic
;; keybindings, but a better default.
(use-package emacs
  :ensure nil
  :bind
  ([remap capitalize-word] . capitalize-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap upcase-word] . upcase-dwim))

;; Title case
;; Gosh, I wish I’d had this when I was so active on MusicBrainz.
(use-package titlecase
  :defer t)

;; Jinx is a just-in-time spell checker.
(use-package jinx
  :ensure t
  :diminish
  ;; I don't want it anywhere except I really want.
  ;; :hook (on-first-buffer . global-jinx-mode)
  :bind
  ([remap ispell-word] . jinx-correct)
  :bind
  (:map ltl/toggles-map
   ("$" . jinx-mode)))

(provide 'init-text)
;; init-text.el ends here

