;;; init-shell.el --- Terminal and shell configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures terminal emulators and shell integration:
;; - vterm: libvterm-backed TTY for full-screen TUIs (AI agent CLI)
;; - eat: Elisp terminal emulator for ordinary shells and Eshell
;; - eshell: Built-in Emacs shell with custom prompt
;;
;; vterm needs a compiled `vterm-module'.  `ltl/vterm-setup' downloads
;; the package via Elpaca, installs compile tools on Ubuntu/Arch, and
;; compiles the module on first use (`vterm-always-compile-module').
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup ltl-vterm nil
  "Vterm install and compile helpers."
  :group 'tools
  :prefix "ltl/vterm-")

(defvar ltl/vterm-build-tools
  '(("cmake" . "CMake (required to compile vterm-module)")
    ("make" . "Make (required to compile vterm-module)")
    ("gcc" . "C compiler (required to compile vterm-module)")
    ("libtool" . "libtool / libtool-bin (needed when system libvterm is missing)"))
  "Build tools required to compile `vterm-module'.
Each entry is (EXECUTABLE . DESCRIPTION).  `libtool' also matches
`libtoolize' / `glibtool'.")

(defun ltl/vterm--libtool-available-p ()
  "Return non-nil if a libtool executable is on PATH."
  (or (executable-find "libtool")
      (executable-find "glibtool")
      (executable-find "libtoolize")))

(defun ltl/vterm--missing-build-tools ()
  "Return descriptions of missing vterm build tools."
  (let ((missing nil))
    (dolist (cell ltl/vterm-build-tools)
      (let ((exe (car cell)))
        (unless (if (string= exe "libtool")
                    (ltl/vterm--libtool-available-p)
                  (executable-find exe))
          (push (cdr cell) missing))))
    (nreverse missing)))

(defvar ltl/vterm-debian-packages
  '("build-essential" "cmake" "libtool-bin" "libvterm-dev")
  "apt packages needed to compile `vterm-module' on Debian/Ubuntu.")

(defvar ltl/vterm-arch-packages
  '("base-devel" "cmake" "libtool" "libvterm")
  "pacman packages needed to compile `vterm-module' on Arch Linux.")

(defun ltl/vterm--os-release ()
  "Return (ID . ID_LIKE) from /etc/os-release, or nil."
  (let ((file "/etc/os-release"))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let (id like)
          (goto-char (point-min))
          (when (re-search-forward "^ID=\"?\\([^\"\n]+\\)\"?" nil t)
            (setq id (downcase (match-string 1))))
          (goto-char (point-min))
          (when (re-search-forward "^ID_LIKE=\"?\\([^\"\n]+\\)\"?" nil t)
            (setq like (downcase (match-string 1))))
          (cons id like))))))

(defun ltl/vterm--distro ()
  "Return `debian', `arch', or nil for the current Linux distro.
`debian' covers Ubuntu and other apt-based systems.  `arch' covers
Arch Linux and pacman-based derivatives."
  (let* ((rel (ltl/vterm--os-release))
         (id (car rel))
         (like (split-string (or (cdr rel) "") "[ \t]+" t)))
    (cond
     ((member id '("ubuntu" "debian" "pop" "linuxmint" "elementary"
                   "raspbian" "neon" "zorin" "kali"))
      'debian)
     ((member id '("arch" "manjaro" "endeavouros" "garuda" "artix" "cachyos"))
      'arch)
     ((member "ubuntu" like) 'debian)
     ((member "debian" like) 'debian)
     ((member "arch" like) 'arch)
     ((executable-find "apt-get") 'debian)
     ((executable-find "pacman") 'arch))))

(defun ltl/vterm--install-hint ()
  "Return a distro-specific command that installs vterm build deps."
  (pcase (ltl/vterm--distro)
    ('debian
     (concat "sudo apt-get install -y "
             (mapconcat #'identity ltl/vterm-debian-packages " ")))
    ('arch
     (concat "sudo pacman -S --needed --noconfirm "
             (mapconcat #'identity ltl/vterm-arch-packages " ")))
    (_
     (cond
      ((and (boundp 'sys/macp) sys/macp)
       "brew install cmake libtool libvterm")
      ((and (boundp 'sys/win32p) sys/win32p)
       "install cmake, make, gcc, and libtool (MSYS2/WSL)")
      (t
       "sudo apt-get install -y build-essential cmake libtool-bin libvterm-dev")))))

(defun ltl/vterm--privileged-argv ()
  "Return (PROGRAM . ARGS) that installs vterm build deps as root.
Nil when this distro is not handled automatically."
  (pcase (ltl/vterm--distro)
    ('debian
     (cons "sh"
           (list "-c"
                 (concat "apt-get update && apt-get install -y "
                         (mapconcat #'identity ltl/vterm-debian-packages " ")))))
    ('arch
     (cons "pacman"
           (append '("-S" "--noconfirm" "--needed")
                   ltl/vterm-arch-packages)))))

(defun ltl/vterm--sudo-nopass-p ()
  "Return non-nil if `sudo -n' can run without a password."
  (and (executable-find "sudo")
       (zerop (call-process "sudo" nil nil nil "-n" "true"))))

(defun ltl/vterm--run-privileged (program args)
  "Run PROGRAM ARGS as root.  Return exit status.
Writes command output to `*vterm-setup*'.  Uses passwordless sudo
when available, otherwise prompts once via `read-passwd'."
  (let ((buf (get-buffer-create "*vterm-setup*"))
        (status 1))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "+ %s %s\n"
                      program (mapconcat #'identity args " ")))
      (cond
       ((zerop (user-uid))
        (setq status (apply #'call-process program nil buf t args)))
       ((ltl/vterm--sudo-nopass-p)
        (setq status (apply #'call-process "sudo" nil buf t program args)))
       ((not (executable-find "sudo"))
        (insert "sudo is not available\n"))
       (noninteractive
        (insert "Cannot prompt for sudo in batch mode\n"))
       (t
        (let ((pass (read-passwd "sudo password (install vterm build tools): ")))
          (cond
           ((or (null pass) (string-empty-p pass))
            (insert "Cancelled: empty sudo password\n"))
           (t
            (setq status
                  (with-temp-buffer
                    (insert pass "\n")
                    (apply #'call-process-region
                           (point-min) (point-max)
                           "sudo" t buf nil
                           (append (list "-S" "-p" "")
                                   (cons program args)))))
            (clear-string pass)))))))
    (display-buffer buf)
    status))

(defun ltl/vterm--install-build-tools ()
  "Install vterm compile tools via apt or pacman when possible.
Return non-nil on success.  Other distros are left to the user."
  (let ((argv (ltl/vterm--privileged-argv)))
    (unless argv
      (user-error
       "vterm build tools missing, and this OS is not auto-installed. Run: %s"
       (ltl/vterm--install-hint)))
    (message "Installing vterm build tools (%s)..." (ltl/vterm--distro))
    (let ((status (ltl/vterm--run-privileged (car argv) (cdr argv))))
      (unless (and (numberp status) (zerop status))
        (user-error
         "Failed to install vterm build tools (exit %s). See *vterm-setup* or run: %s"
         status
         (ltl/vterm--install-hint)))
      (message "vterm build tools installed")
      t)))

(defun ltl/vterm--module-loaded-p ()
  "Return non-nil if the native `vterm-module' is already loaded."
  (featurep 'vterm-module))

(defun ltl/vterm--assert-emacs-modules ()
  "Signal a user-error if this Emacs cannot load dynamic modules."
  (unless (and (boundp 'module-file-suffix) module-file-suffix)
    (user-error
     "This Emacs was built without dynamic modules; vterm cannot load")))

(defun ltl/vterm--ensure-build-tools ()
  "Install missing compile tools on Ubuntu/Arch, else signal a user-error."
  (when (ltl/vterm--missing-build-tools)
    (if (ltl/vterm--privileged-argv)
        (ltl/vterm--install-build-tools)
      (user-error
       "vterm cannot compile (%s missing). Install with: %s"
       (mapconcat #'identity (ltl/vterm--missing-build-tools) "; ")
       (ltl/vterm--install-hint))))
  (when-let* ((missing (ltl/vterm--missing-build-tools)))
    (user-error
     "vterm still missing %s after install. See *vterm-setup* or run: %s"
     (mapconcat #'identity missing "; ")
     (ltl/vterm--install-hint))))

(defun ltl/vterm--wait-for-package ()
  "Block until Elpaca has installed the vterm package."
  (when (and (not (locate-library "vterm"))
             (fboundp 'elpaca-wait))
    (message "Installing vterm via Elpaca...")
    (elpaca-wait))
  (unless (locate-library "vterm")
    (user-error
     "vterm is not installed. Check *elpaca-log*, or run M-x ltl/vterm-setup")))

;;;###autoload
(defun ltl/vterm-ready-p ()
  "Return non-nil when vterm and `vterm-module' can be used."
  (and (locate-library "vterm")
       (or (ltl/vterm--module-loaded-p)
           (locate-library "vterm-module"))))

;;;###autoload
(defun ltl/vterm-ensure ()
  "Make vterm and its native module available, compiling if needed.
Called by `M-x vterm' (via package load) and by the AI agent pane.
Ubuntu/Debian and Arch install missing compile tools first."
  (ltl/vterm--assert-emacs-modules)
  (ltl/vterm--wait-for-package)
  (setq vterm-always-compile-module t)
  (unless (featurep 'vterm)
    (when (and (not (locate-library "vterm-module"))
               (not (ltl/vterm--module-loaded-p)))
      (ltl/vterm--ensure-build-tools)
      (message "Compiling vterm-module (first time only)..."))
    (require 'vterm))
  (unless (or (featurep 'vterm-module) (locate-library "vterm-module"))
    (user-error
     "vterm-module did not load. See buffer %s, or run: %s"
     (if (boundp 'vterm-install-buffer-name)
         vterm-install-buffer-name
       " *Install vterm* ")
     (ltl/vterm--install-hint)))
  t)

;;;###autoload
(defun ltl/vterm-setup ()
  "Install vterm via Elpaca and compile `vterm-module' if needed.
On Ubuntu/Debian and Arch Linux, missing compile tools are installed
with apt or pacman (sudo password may be requested).
Safe to re-run.  Prints a short status in the echo area."
  (interactive)
  (ltl/vterm-ensure)
  (message "vterm ready (%s)"
           (or (locate-library "vterm-module")
               (locate-library "vterm"))))

;; vterm - libvterm terminal emulator
;; Agent TUIs (cursor-agent / Claude / qodercli) run here so PTY work
;; stays in C instead of Eat's Elisp emulator, which can freeze Emacs.
(use-package vterm
  :ensure (vterm :host github
                 :repo "akermu/emacs-libvterm"
                 :files ("CMakeLists.txt"
                         "elisp.c" "elisp.h"
                         "emacs-module.h"
                         "etc"
                         "utf8.c" "utf8.h"
                         "vterm.el"
                         "vterm-module.c" "vterm-module.h"))
  :defer t
  :commands (vterm vterm-other-window vterm-module-compile)
  :init
  ;; Must be set before `vterm.el' loads: that file compiles the module
  ;; at require-time when the .so is missing.
  (setq vterm-always-compile-module t)
  (when (boundp 'ltl/tools-map)
    (define-key ltl/tools-map (kbd "v") #'vterm))
  :custom
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit nil)
  (vterm-min-window-width 40))

;; eat - Emulate A Terminal
;; Kept for ordinary interactive shells and Eshell visual commands.
;; Full-screen agent TUIs use vterm instead (`init-agent-cli').
(use-package eat
  :ensure (eat :host codeberg
               :repo "akib/emacs-eat"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       ("e" "e/*")))
  ;; Reuse Eat inside Eshell so shell commands with terminal control codes
  ;; render correctly instead of degrading to plain text output.
  :hook (eshell-load . eat-eshell-mode)
  :config
  ;; TTY redisplay of Eat is much more expensive than on GUI.  Stretch
  ;; the output coalescing window so a burst of CSI sequences becomes
  ;; one paint instead of dozens.
  (unless (display-graphic-p)
    (setq eat-enable-mouse nil
          eat-enable-blinking-text nil
          eat-minimum-latency 0.05
          eat-maximum-latency 0.20)))

;; Eshell configuration
;; `eshell' stays useful as a Lisp-native shell for quick project-local
;; commands, especially when you want buffer integration over a full TTY.
(use-package eshell
  :ensure nil
  :defer t
  :custom
  ;; Keep the prompt/input visible during long command output.
  (eshell-scroll-to-bottom-on-input 'all)
  ;; Auto-clean finished process buffers so temporary shell commands do
  ;; not leave dead buffers lying around.
  (eshell-destroy-buffer-when-process-dies t)
  ;; Skip duplicate history entries to make shell recall more meaningful.
  (eshell-hist-ignoredups t)
  ;; Persist history between sessions so project shell commands are easy to revisit.
  (eshell-save-history-on-exit t)
  :config
  ;; Show cwd and current Git branch in the prompt so project context is
  ;; obvious even when several shell buffers are open.
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize (abbreviate-file-name (eshell/pwd)) 'face 'font-lock-keyword-face)
           (when (and (fboundp 'magit-get-current-branch) (magit-get-current-branch))
             (propertize (concat " " (magit-get-current-branch)) 'face 'font-lock-string-face))
           (propertize " λ " 'face 'font-lock-constant-face))))
  ;; Match the custom prompt precisely so Eshell can recognize prompt boundaries.
  (setq eshell-prompt-regexp "^.* λ "))

(provide 'init-shell)
;;; init-shell.el ends here
