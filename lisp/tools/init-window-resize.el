;;; init-window-resize.el --- Persistent window resize menu -*- lexical-binding: t -*-

;;; Commentary:
;;
;; A lazy-loaded Transient menu for the resize commands defined in
;; `init-bindings'.  Every resize suffix stays transient so the menu
;; remains visible at the bottom until `q' or `C-g' exits.

;;; Code:

(require 'transient)

(declare-function ltl/window-narrower "init-bindings")
(declare-function ltl/window-wider "init-bindings")
(declare-function ltl/window-shorter "init-bindings")
(declare-function ltl/window-taller "init-bindings")
(declare-function ltl/window-balance "init-bindings")
(declare-function ltl/window-agent-preset "init-bindings")
(declare-function ltl/window-treemacs-preset "init-bindings")

;;;###autoload
(transient-define-prefix ltl/window-resize-menu ()
  "Resize Emacs windows while keeping this help menu visible."
  [["Selected window"
    ("h" "narrower" ltl/window-narrower :transient t)
    ("l" "wider" ltl/window-wider :transient t)
    ("j" "shorter" ltl/window-shorter :transient t)
    ("k" "taller" ltl/window-taller :transient t)]
   ["Layout / presets"
    ("=" "balance" ltl/window-balance :transient t)
    ("a" "Agent width" ltl/window-agent-preset :transient t)
    ("t" "Treemacs width" ltl/window-treemacs-preset :transient t)
    ("q" "quit" transient-quit-one)]])

(provide 'init-window-resize)
;;; init-window-resize.el ends here
