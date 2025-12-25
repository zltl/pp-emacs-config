;;; init-copilot-cli.el --- GitHub Copilot CLI integration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Deep integration with GitHub Copilot CLI (the `copilot` command).
;; Features:
;; - Uses eat terminal emulator for proper terminal handling
;; - Send current buffer/region to CLI with @file reference
;; - Auto project directory detection
;; - Side-by-side layout with source code
;;
;; Requirements:
;; - GitHub Copilot CLI installed (`copilot` command available)
;; - Authenticated with GitHub
;; - eat package for terminal emulation
;;
;;; Code:

(require 'eat nil t)

(defgroup copilot-cli nil
  "GitHub Copilot CLI integration."
  :group 'tools
  :prefix "copilot-cli-")

(defcustom copilot-cli-executable "copilot"
  "Path to the Copilot CLI executable."
  :type 'string
  :group 'copilot-cli)

(defcustom copilot-cli-buffer-name "*Copilot-CLI*"
  "Name of the Copilot CLI buffer."
  :type 'string
  :group 'copilot-cli)

(defcustom copilot-cli-model "claude-sonnet-4"
  "Default AI model to use."
  :type '(choice (const "claude-sonnet-4")
                 (const "claude-sonnet-4.5")
                 (const "gpt-5")
                 (const "gpt-4.1")
                 (const "o3")
                 (const "o4-mini"))
  :group 'copilot-cli)

(defcustom copilot-cli-window-width 100
  "Width of the Copilot CLI window."
  :type 'integer
  :group 'copilot-cli)

(defcustom copilot-cli-extra-args nil
  "Extra arguments to pass to copilot CLI."
  :type '(repeat string)
  :group 'copilot-cli)

;;; Variables
(defvar copilot-cli--source-buffer nil
  "The source buffer that initiated the CLI session.")

(defvar copilot-cli--eat-buffer nil
  "The eat terminal buffer running Copilot CLI.")

;;; Helper functions
(defun copilot-cli--project-root ()
  "Get the project root directory."
  (or (when (fboundp 'project-root)
        (when-let ((proj (project-current)))
          (project-root proj)))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun copilot-cli--build-command ()
  "Build the copilot CLI command with arguments."
  (let ((args (list copilot-cli-executable
                    "--model" copilot-cli-model)))
    (when copilot-cli-extra-args
      (setq args (append args copilot-cli-extra-args)))
    (mapconcat #'identity args " ")))

(defun copilot-cli--setup-windows ()
  "Setup the window layout for Copilot CLI."
  (let* ((source-win (selected-window))
         (width-to-keep (- (window-width) copilot-cli-window-width)))
    ;; Split window, keeping source on left
    (split-window-right width-to-keep)
    ;; Stay in source window (left side)
    (select-window source-win)))

;;;###autoload
(defun copilot-cli ()
  "Start or switch to a Copilot CLI session using eat terminal."
  (interactive)
  (unless (executable-find copilot-cli-executable)
    (user-error "Copilot CLI not found. Please install GitHub Copilot CLI"))
  (unless (fboundp 'eat)
    (user-error "eat package not available. Please install eat terminal emulator"))
  (setq copilot-cli--source-buffer (current-buffer))
  (let ((default-directory (copilot-cli--project-root)))
    ;; Check if CLI buffer already exists and is alive
    (if (and copilot-cli--eat-buffer
             (buffer-live-p copilot-cli--eat-buffer)
             (get-buffer-process copilot-cli--eat-buffer))
        ;; Switch to existing buffer in right window
        (copilot-cli--show-buffer)
      ;; Start new session
      (copilot-cli--start-eat))))

(defun copilot-cli--start-eat ()
  "Start Copilot CLI in eat terminal."
  (let* ((default-directory (copilot-cli--project-root))
         (cmd (copilot-cli--build-command))
         (source-win (selected-window)))
    ;; Setup window layout first
    (copilot-cli--setup-windows)
    ;; Move to right window and start eat there
    (other-window 1)
    (let ((buf (eat cmd t)))
      (setq copilot-cli--eat-buffer buf)
      (with-current-buffer buf
        (rename-buffer copilot-cli-buffer-name t)
        ;; Add local keybindings
        (local-set-key (kbd "C-c C-q") #'copilot-cli-quit)
        (local-set-key (kbd "C-c C-s") #'copilot-cli-send-file)
        (local-set-key (kbd "C-c C-b") #'copilot-cli-insert-source-file)
        ;; Ensure mode switching keys are available
        (local-set-key (kbd "C-c C-j") #'eat-semi-char-mode)
        (local-set-key (kbd "C-c C-e") #'eat-emacs-mode)
        ;; Start in semi-char mode (input mode)
        (eat-semi-char-mode)))
    ;; Return to source window
    (select-window source-win)))

(defun copilot-cli--show-buffer ()
  "Show the Copilot CLI buffer in right window."
  (when copilot-cli--eat-buffer
    (let ((cli-win (get-buffer-window copilot-cli--eat-buffer)))
      (if cli-win
          ;; Buffer already visible, just select it if needed
          nil
        ;; Need to display it - setup windows
        (copilot-cli--setup-windows)
        (other-window 1)
        (switch-to-buffer copilot-cli--eat-buffer)
        (other-window -1)))))

;;; Terminal interaction helpers
(defun copilot-cli--send-to-terminal (text)
  "Send TEXT to the eat terminal."
  (when (and copilot-cli--eat-buffer
             (buffer-live-p copilot-cli--eat-buffer))
    (with-current-buffer copilot-cli--eat-buffer
      (when-let ((proc (get-buffer-process (current-buffer))))
        (process-send-string proc text)))))

(defun copilot-cli-insert-source-file ()
  "Insert @file reference to the source buffer in terminal."
  (interactive)
  (when (and copilot-cli--source-buffer
             (buffer-file-name copilot-cli--source-buffer))
    (copilot-cli--send-to-terminal
     (format "@%s " (buffer-file-name copilot-cli--source-buffer)))))

(defun copilot-cli-send-file (file)
  "Insert @FILE reference in terminal."
  (interactive "fFile: ")
  (copilot-cli--send-to-terminal (format "@%s " (expand-file-name file))))

;;; Quick commands - these open CLI and pre-fill input
;;;###autoload
(defun copilot-cli-send-buffer ()
  "Open Copilot CLI with current buffer as context."
  (interactive)
  (let ((file (buffer-file-name)))
    (when file (save-buffer))
    (copilot-cli)
    (run-at-time 0.5 nil
                 (lambda ()
                   (when file
                     (copilot-cli--send-to-terminal (format "@%s " file)))))))

;;;###autoload
(defun copilot-cli-send-region (start end)
  "Send region from START to END to Copilot CLI."
  (interactive "r")
  (let* ((content (buffer-substring-no-properties start end))
         (ext (or (file-name-extension (buffer-name) t) ".txt"))
         (temp-file (make-temp-file "copilot-region-" nil ext)))
    (with-temp-file temp-file
      (insert content))
    (copilot-cli)
    (run-at-time 0.5 nil
                 (lambda ()
                   (copilot-cli--send-to-terminal (format "@%s " temp-file))))))

;;;###autoload
(defun copilot-cli-explain ()
  "Ask Copilot CLI to explain code."
  (interactive)
  (if (use-region-p)
      (copilot-cli-send-region (region-beginning) (region-end))
    (copilot-cli-send-buffer))
  (run-at-time 1.0 nil
               (lambda ()
                 (copilot-cli--send-to-terminal "请解释这段代码\n"))))

;;;###autoload
(defun copilot-cli-review ()
  "Ask Copilot CLI to review code."
  (interactive)
  (copilot-cli-send-buffer)
  (run-at-time 1.0 nil
               (lambda ()
                 (copilot-cli--send-to-terminal
                  "请审查这段代码，找出潜在问题和改进建议\n"))))

;;;###autoload
(defun copilot-cli-fix-error ()
  "Send buffer with errors to Copilot CLI."
  (interactive)
  (let ((errors (copilot-cli--collect-errors)))
    (copilot-cli-send-buffer)
    (run-at-time 1.0 nil
                 (lambda ()
                   (if errors
                       (copilot-cli--send-to-terminal
                        (format "请修复以下错误:\n%s\n" errors))
                     (copilot-cli--send-to-terminal
                      "请检查并修复代码中的错误\n"))))))

(defun copilot-cli--collect-errors ()
  "Collect errors from flycheck or flymake."
  (with-current-buffer (or copilot-cli--source-buffer (current-buffer))
    (cond
     ((bound-and-true-p flycheck-mode)
      (mapconcat
       (lambda (err)
         (format "Line %d: %s"
                 (flycheck-error-line err)
                 (flycheck-error-message err)))
       (flycheck-overlay-errors-in (point-min) (point-max))
       "\n"))
     ((bound-and-true-p flymake-mode)
      (mapconcat
       (lambda (diag)
         (format "Line %d: %s"
                 (line-number-at-pos (flymake-diagnostic-beg diag))
                 (flymake-diagnostic-text diag)))
       (flymake-diagnostics)
       "\n"))
     (t nil))))

;;;###autoload
(defun copilot-cli-generate-tests ()
  "Ask Copilot CLI to generate tests."
  (interactive)
  (copilot-cli-send-buffer)
  (run-at-time 1.0 nil
               (lambda ()
                 (copilot-cli--send-to-terminal
                  "请为这个文件生成单元测试\n"))))

;;;###autoload
(defun copilot-cli-refactor (instruction)
  "Refactor with INSTRUCTION."
  (interactive "sRefactor instruction: ")
  (copilot-cli-send-buffer)
  (run-at-time 1.0 nil
               (lambda ()
                 (copilot-cli--send-to-terminal
                  (format "请重构: %s\n" instruction)))))

;;; Process control
;;;###autoload
(defun copilot-cli-stop ()
  "Stop the Copilot CLI process."
  (interactive)
  (when (and copilot-cli--eat-buffer
             (buffer-live-p copilot-cli--eat-buffer))
    (when-let ((proc (get-buffer-process copilot-cli--eat-buffer)))
      (delete-process proc))))

;;;###autoload
(defun copilot-cli-restart ()
  "Restart the Copilot CLI session."
  (interactive)
  (copilot-cli-quit)
  (copilot-cli))

;;;###autoload
(defun copilot-cli-quit ()
  "Quit Copilot CLI and close windows."
  (interactive)
  (copilot-cli-stop)
  (when (and copilot-cli--eat-buffer
             (buffer-live-p copilot-cli--eat-buffer))
    (kill-buffer copilot-cli--eat-buffer))
  (setq copilot-cli--eat-buffer nil)
  (delete-other-windows))

;;; Keybindings
(with-eval-after-load 'bind-key
  (when (boundp 'ltl/toggles-map)
    (define-key ltl/toggles-map (kbd "C") #'copilot-cli)))

;; Define prefix map for Copilot CLI commands
(defvar copilot-cli-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'copilot-cli)
    (define-key map (kbd "s") #'copilot-cli-send-buffer)
    (define-key map (kbd "r") #'copilot-cli-send-region)
    (define-key map (kbd "e") #'copilot-cli-explain)
    (define-key map (kbd "v") #'copilot-cli-review)
    (define-key map (kbd "f") #'copilot-cli-fix-error)
    (define-key map (kbd "t") #'copilot-cli-generate-tests)
    (define-key map (kbd "R") #'copilot-cli-refactor)
    (define-key map (kbd "k") #'copilot-cli-stop)
    (define-key map (kbd "x") #'copilot-cli-restart)
    (define-key map (kbd "q") #'copilot-cli-quit)
    map)
  "Keymap for Copilot CLI commands.")

(global-set-key (kbd "C-c c") copilot-cli-command-map)

(provide 'init-copilot-cli)
;;; init-copilot-cli.el ends here
