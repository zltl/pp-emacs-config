;;; init-agent-cli.el --- Multi-backend AI agent CLI in eat -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Side-by-side AI agent workflow using eat, with switchable backends and
;; multiple concurrent sessions:
;; - qodercli
;; - cursor-agent (`agent` / `cursor-agent`)
;; - claude-code (`claude`), with a one-shot dangerous/skip-permissions launch
;;
;; Use `C-c c l' to list sessions and select one; `C-c c n' starts another
;; session in the agent pane (existing sessions stay alive).
;; `C-c c a' switches the default backend and remembers it across restarts.
;; `C-c c c' opens that saved backend, not whichever session happens to be current.
;;
;;; Code:

(require 'eat nil t)
(require 'cl-lib)
(require 'subr-x)

(defgroup ltl-agent nil
  "Eat-backed AI agent CLI integration."
  :group 'tools
  :prefix "ltl/agent-")

(defcustom ltl/agent-backend nil
  "Default AI agent CLI backend for new sessions.
Nil means auto-pick the first installed backend from `ltl/agent-backends'.
`C-c c a' saves this choice so it is restored after restart."
  :type '(choice (const :tag "Auto (first installed)" nil)
                 (const qodercli)
                 (const cursor-agent)
                 (const claude-code))
  :group 'ltl-agent)

(defun ltl/agent--state-file ()
  "File used to remember the last selected agent backend."
  (expand-file-name "agent-backend"
                    (if (boundp 'ltl/var-directory)
                        ltl/var-directory
                      (expand-file-name "var/" user-emacs-directory))))

(defun ltl/agent--save-backend (backend)
  "Remember BACKEND as the default and persist it for the next Emacs session."
  (setq ltl/agent-backend backend)
  (let ((file (ltl/agent--state-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (format "%S\n" backend)))))

(defun ltl/agent--load-backend ()
  "Restore the last selected agent backend, if one was saved."
  (let ((file (ltl/agent--state-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((val (ignore-errors (read (current-buffer)))))
          (when (and val (assq val ltl/agent-backends))
            (setq ltl/agent-backend val)))))))

(defcustom ltl/agent-backends
  '((qodercli
     :command "qodercli"
     :buffer-prefix "*Qoder-CLI*"
     :args nil
     :file-ref "@%s ")
    (cursor-agent
     :command "agent"
     :fallback-command "cursor-agent"
     :buffer-prefix "*Cursor-Agent*"
     :args nil
     :file-ref "@%s ")
    (claude-code
     :command "claude"
     :buffer-prefix "*Claude-Code*"
     :args nil
     :file-ref "@%s "))
  "Backend definitions for `ltl/agent-backend'.
Each entry is (SYMBOL . PLIST) with keys:
:command, optional :fallback-command, :buffer-prefix, :args, :file-ref."
  :type '(alist :key-type symbol :value-type plist)
  :group 'ltl-agent)

(defcustom ltl/agent-claude-skip-permissions nil
  "If non-nil, always pass --dangerously-skip-permissions to Claude Code."
  :type 'boolean
  :group 'ltl-agent)

(defcustom ltl/agent-window-width 100
  "Width of the agent CLI side window."
  :type 'integer
  :group 'ltl-agent)

(defcustom ltl/agent-directory nil
  "Override working directory for new agent sessions.
Nil means auto-detect via project.el / .git / `default-directory'."
  :type '(choice (const :tag "Auto" nil)
                 (directory :tag "Directory"))
  :group 'ltl-agent)

(defcustom ltl/agent-prompt-explain
  "Please explain this code\n"
  "Prompt sent when requesting code explanation."
  :type 'string
  :group 'ltl-agent)

(defcustom ltl/agent-prompt-review
  "Please review this code, find potential issues and suggest improvements\n"
  "Prompt sent when requesting code review."
  :type 'string
  :group 'ltl-agent)

(defcustom ltl/agent-prompt-fix
  "Please fix the following errors:\n%s\n"
  "Prompt sent when requesting error fixes. Must contain %s for error text."
  :type 'string
  :group 'ltl-agent)

(defcustom ltl/agent-prompt-fix-general
  "Please check and fix errors in the code\n"
  "Prompt sent when requesting a general error check."
  :type 'string
  :group 'ltl-agent)

(defcustom ltl/agent-prompt-test
  "Please generate unit tests for this file\n"
  "Prompt sent when requesting test generation."
  :type 'string
  :group 'ltl-agent)

(defcustom ltl/agent-prompt-refactor
  "Please refactor: %s\n"
  "Prompt sent when requesting refactoring. Must contain %s for instruction."
  :type 'string
  :group 'ltl-agent)

(defvar ltl/agent--sessions nil
  "List of live agent sessions.
Each session is a plist:
:id :backend :buffer :root :skip-permissions :label :created.")

(defvar ltl/agent--current nil
  "Currently selected agent session plist.")

(defvar ltl/agent--source-buffer nil
  "Source buffer that initiated the latest agent action.")

(defvar ltl/agent--next-extra-args nil
  "Extra CLI args applied to the next launched session only.")

(defvar ltl/agent--next-skip-permissions nil
  "If non-nil, next Claude session gets --dangerously-skip-permissions.")

(defvar ltl/agent--id-counter 0
  "Monotonic session id counter.")

(defun ltl/agent--backend-plist (&optional backend)
  "Return plist for BACKEND or the resolved default backend."
  (let ((backend (or backend (ltl/agent--resolved-backend) ltl/agent-backend)))
    (or (alist-get backend ltl/agent-backends)
        (user-error "Unknown agent backend: %s" backend))))

(defun ltl/agent--executable (&optional backend)
  "Resolve executable path for BACKEND."
  (let* ((plist (or (alist-get (or backend ltl/agent-backend) ltl/agent-backends)
                    (and backend (ltl/agent--backend-plist backend))))
         (cmd (plist-get plist :command))
         (fallback (plist-get plist :fallback-command)))
    (when plist
      (or (and cmd (executable-find cmd))
          (and fallback (executable-find fallback))))))

(defun ltl/agent--available-backends ()
  "Return backend symbols that have an executable on PATH."
  (cl-loop for (backend . _plist) in ltl/agent-backends
           when (ltl/agent--executable backend)
           collect backend))

(defun ltl/agent--describe-installs ()
  "Return a short string describing installed backends."
  (let ((available (ltl/agent--available-backends)))
    (if available
        (mapconcat
         (lambda (b)
           (format "%s (%s)" b (ltl/agent--executable b)))
         available
         ", ")
      "none")))

(defun ltl/agent--resolved-backend (&optional backend)
  "Return BACKEND if installed, else the preferred installed backend.
Does not overwrite `ltl/agent-backend'. Signals a user-error when
nothing usable is installed."
  (let* ((available (ltl/agent--available-backends))
         (want (or backend ltl/agent-backend)))
    (cond
     ((null available)
      (user-error
       "No AI agent CLI found on PATH. Install one of: %s"
       (mapconcat
        (lambda (b)
          (let ((p (alist-get b ltl/agent-backends)))
            (or (plist-get p :command)
                (symbol-name b))))
        (mapcar #'car ltl/agent-backends)
        ", ")))
     ((and want (memq want available)) want)
     ((and want (not (memq want available)))
      (let ((fallback (car available)))
        (message "Agent %s not installed; using %s instead (installed: %s)"
                 want fallback (ltl/agent--describe-installs))
        fallback))
     (t
      (car available)))))

(defun ltl/agent--find-session (backend)
  "Return a live session for BACKEND, or nil."
  (cl-find-if (lambda (s)
                (and (eq (plist-get s :backend) backend)
                     (ltl/agent--session-alive-p s)))
              ltl/agent--sessions))

;;;###autoload
(defun ltl/agent-status ()
  "Show which AI agent CLIs are installed and the current default."
  (interactive)
  (let* ((available (ltl/agent--available-backends))
         (lines
          (mapcar
           (lambda (cell)
             (let* ((backend (car cell))
                    (exe (ltl/agent--executable backend)))
               (format "  %s %-14s %s"
                       (if exe "[ok]" "[--]")
                       backend
                       (or exe "not found"))))
           ltl/agent-backends)))
    (message "Agent default: %s | installed: %s\n%s"
             (or ltl/agent-backend "auto")
             (if available
                 (mapconcat #'symbol-name available ", ")
                 "none")
             (mapconcat #'identity lines "\n"))))

(defun ltl/agent--file-ref (file &optional backend)
  "Format FILE reference for BACKEND."
  (format (or (plist-get (ltl/agent--backend-plist backend) :file-ref) "@%s ")
          (expand-file-name file)))

(defun ltl/agent--project-root (&optional directory)
  "Return working directory for new agent sessions.
If DIRECTORY is non-nil, use it. Otherwise prefer `ltl/agent-directory',
then project.el / .git / `default-directory'."
  (or directory
      (and ltl/agent-directory
           (file-directory-p ltl/agent-directory)
           (expand-file-name ltl/agent-directory))
      (when (fboundp 'project-current)
        (when-let* ((proj (project-current nil default-directory)))
          (project-root proj)))
      (locate-dominating-file default-directory ".git")
      default-directory))

;;;###autoload
(defun ltl/agent-set-directory (directory)
  "Set working directory for new agent sessions to DIRECTORY.
With a prefix argument, clear the override and go back to auto-detect.
Existing sessions keep their own roots; start a new one (`C-c c n') to use
the new directory."
  (interactive
   (if current-prefix-arg
       (list nil)
     (list (read-directory-name
            (format "Agent workdir (current: %s): "
                    (abbreviate-file-name
                     (or ltl/agent-directory
                         (ltl/agent--project-root)
                         default-directory)))
            (or ltl/agent-directory
                (ltl/agent--project-root)
                default-directory)
            nil t))))
  (setq ltl/agent-directory
        (and directory (expand-file-name directory)))
  (if ltl/agent-directory
      (message "Agent workdir: %s (new sessions only)"
               (abbreviate-file-name ltl/agent-directory))
    (message "Agent workdir: auto (%s)"
             (abbreviate-file-name (ltl/agent--project-root)))))

(defun ltl/agent--prune-sessions ()
  "Drop sessions whose buffers are dead."
  (setq ltl/agent--sessions
        (cl-remove-if-not
         (lambda (s)
           (buffer-live-p (plist-get s :buffer)))
         ltl/agent--sessions))
  (when (and ltl/agent--current
             (not (buffer-live-p (plist-get ltl/agent--current :buffer))))
    (setq ltl/agent--current (car ltl/agent--sessions))))

(defun ltl/agent--session-alive-p (session)
  "Return non-nil if SESSION buffer and process are alive."
  (and session
       (buffer-live-p (plist-get session :buffer))
       (get-buffer-process (plist-get session :buffer))))

(defun ltl/agent--session-label (session)
  "Human-readable label for SESSION."
  (or (plist-get session :label)
      (format "#%s %s%s @ %s"
              (plist-get session :id)
              (plist-get session :backend)
              (if (plist-get session :skip-permissions) " [yolo]" "")
              (abbreviate-file-name (or (plist-get session :root) "~")))))

(defun ltl/agent--make-buffer-name (backend skip)
  "Unique buffer name for BACKEND, optionally SKIP permissions."
  (let* ((plist (ltl/agent--backend-plist backend))
         (base (or (plist-get plist :buffer-prefix)
                   (plist-get plist :buffer-name)
                   "*Agent-CLI*"))
         (base (if (and (eq backend 'claude-code) skip)
                   (replace-regexp-in-string
                    "\\*$" "-YOLO*" base t t)
                 base)))
    (generate-new-buffer-name base)))

(defun ltl/agent--build-command (backend &optional skip extra-args)
  "Build shell command for BACKEND with optional SKIP and EXTRA-ARGS."
  (let* ((backend (ltl/agent--resolved-backend backend))
         (plist (alist-get backend ltl/agent-backends))
         (exe (ltl/agent--executable backend))
         (args (append (plist-get plist :args) extra-args)))
    (unless exe
      (user-error "Agent executable not found for backend %s" backend))
    (when (and (eq backend 'claude-code)
               (or skip ltl/agent-claude-skip-permissions)
               (not (member "--dangerously-skip-permissions" args)))
      (setq args (append args '("--dangerously-skip-permissions"))))
    (mapconcat #'identity (cons exe (mapcar #'identity args)) " ")))

(defun ltl/agent--split-agent-window ()
  "Split a right-hand pane for the agent CLI and return the new window.
The selected window is left unchanged."
  (split-window-right (max 20 (- (window-width) ltl/agent-window-width))))

(defun ltl/agent--agent-window ()
  "Return a window already showing an agent session buffer, if any."
  (cl-loop for session in ltl/agent--sessions
           for buf = (plist-get session :buffer)
           when (and (buffer-live-p buf) (get-buffer-window buf))
           return (get-buffer-window buf)))

(defun ltl/agent--bury-other-agent-windows (keep-buf)
  "Remove other live agent buffers from windows, leaving KEEP-BUF."
  (dolist (s ltl/agent--sessions)
    (let ((other (plist-get s :buffer)))
      (when (and (buffer-live-p other)
                 (not (eq other keep-buf))
                 (get-buffer-window other))
        (replace-buffer-in-windows other)))))

(defun ltl/agent--display-session (session &optional select-agent)
  "Display SESSION in the right-hand agent pane.
If SELECT-AGENT is non-nil, leave focus in the agent window.
Reuses an existing agent side window instead of splitting again."
  (when (and session (buffer-live-p (plist-get session :buffer)))
    (setq ltl/agent--current session)
    (let* ((buf (plist-get session :buffer))
           (source (selected-window))
           (agent-win (or (get-buffer-window buf)
                          (ltl/agent--agent-window)
                          (ltl/agent--split-agent-window))))
      (set-window-buffer agent-win buf)
      (ltl/agent--bury-other-agent-windows buf)
      (if select-agent
          (select-window agent-win)
        (when (window-live-p source)
          (select-window source))))))

(defun ltl/agent--register-session (session)
  "Register SESSION and make it current."
  (ltl/agent--prune-sessions)
  (push session ltl/agent--sessions)
  (setq ltl/agent--current session)
  session)

(defun ltl/agent--on-buffer-kill ()
  "Remove killed agent buffers from the session list."
  (when-let* ((buf (current-buffer))
              (session (cl-find-if (lambda (s) (eq (plist-get s :buffer) buf))
                                   ltl/agent--sessions)))
    (setq ltl/agent--sessions (delq session ltl/agent--sessions))
    (when (eq ltl/agent--current session)
      (setq ltl/agent--current (car ltl/agent--sessions)))))

(defun ltl/agent--ensure-eat ()
  "Load Eat. Signal a user-error if it is not available."
  (or (featurep 'eat) (require 'eat nil t))
  (unless (fboundp 'eat)
    (user-error "eat package not available")))

(defun ltl/agent--start-eat (cmd)
  "Run CMD in a new Eat buffer in the selected window."
  (ltl/agent--ensure-eat)
  (let ((display-buffer-overriding-action '(display-buffer-same-window)))
    (eat cmd t)))

(defun ltl/agent--start-session (&optional backend skip keep-existing)
  "Create a new agent session for BACKEND.
SKIP non-nil enables Claude --dangerously-skip-permissions.
Unless KEEP-EXISTING is non-nil, quit the current session first.
The new session is shown in the existing agent pane when possible."
  (ltl/agent--ensure-eat)
  (let ((reuse-win (or (and ltl/agent--current
                            (buffer-live-p (plist-get ltl/agent--current :buffer))
                            (get-buffer-window
                             (plist-get ltl/agent--current :buffer)))
                       (ltl/agent--agent-window)))
        (created-win nil))
    (unless keep-existing
      (when (ltl/agent--session-alive-p ltl/agent--current)
        (ltl/agent-quit 'silent)))
    (let* ((backend (ltl/agent--resolved-backend backend))
           (skip (or skip
                     ltl/agent--next-skip-permissions
                     (and (eq backend 'claude-code)
                          ltl/agent-claude-skip-permissions)))
           (extra ltl/agent--next-extra-args)
           (root (ltl/agent--project-root))
           (cmd (ltl/agent--build-command backend skip extra))
           (source-win (selected-window))
           (source-buf (current-buffer))
           (id (cl-incf ltl/agent--id-counter))
           (name (ltl/agent--make-buffer-name backend skip))
           (agent-win (if (and reuse-win (window-live-p reuse-win))
                          reuse-win
                        (setq created-win (ltl/agent--split-agent-window)))))
      (setq ltl/agent--next-skip-permissions nil
            ltl/agent--next-extra-args nil
            ltl/agent--source-buffer source-buf)
      (condition-case err
          (progn
            (select-window agent-win)
            (let* ((default-directory root)
                   (buf (ltl/agent--start-eat cmd))
                   (session (list :id id
                                  :backend backend
                                  :buffer buf
                                  :root root
                                  :skip-permissions skip
                                  :created (current-time)
                                  :label nil)))
              (with-current-buffer buf
                (rename-buffer name t)
                (setq-local ltl/agent--session-id id)
                (add-hook 'kill-buffer-hook #'ltl/agent--on-buffer-kill nil t)
                (local-set-key (kbd "C-c C-q") #'ltl/agent-quit)
                (local-set-key (kbd "C-c C-s") #'ltl/agent-send-file)
                (local-set-key (kbd "C-c C-b") #'ltl/agent-insert-source-file)
                (local-set-key (kbd "C-c C-j") #'eat-semi-char-mode)
                (local-set-key (kbd "C-c C-e") #'eat-emacs-mode)
                (eat-semi-char-mode))
              (plist-put session :label (ltl/agent--session-label session))
              (ltl/agent--register-session session)
              (ltl/agent--display-session session)
              (when (window-live-p source-win)
                (select-window source-win))
              (message "Agent session %s" (plist-get session :label))
              session))
        (error
         (when (and created-win (window-live-p created-win))
           (delete-window created-win))
         (when (and source-win (window-live-p source-win))
           (select-window source-win))
         (signal (car err) (cdr err)))))))

;;;###autoload
(defun ltl/agent-switch (&optional backend)
  "Switch default agent BACKEND among installed CLIs and persist it.
`C-c c c' afterwards uses this backend, including after restart.
With a prefix argument, also list backends that are not installed."
  (interactive)
  (let* ((available (ltl/agent--available-backends))
         (choices (if current-prefix-arg
                      (mapcar #'car ltl/agent-backends)
                    available)))
    (unless available
      (user-error "No AI agent CLI installed (%s)"
                  (ltl/agent--describe-installs)))
    (unless choices
      (user-error "No agent backends to choose from"))
    (let* ((current (ltl/agent--resolved-backend))
           (next (or backend
                     (intern (completing-read
                              (format "Agent backend (current: %s | installed: %s): "
                                      current
                                      (mapconcat #'symbol-name available ", "))
                              (mapcar #'symbol-name choices)
                              nil t nil nil
                              (symbol-name current))))))
      (unless (assq next ltl/agent-backends)
        (user-error "Unknown agent backend: %s" next))
      (unless (ltl/agent--executable next)
        (user-error "%s is not installed (installed: %s)"
                   next (ltl/agent--describe-installs)))
      (ltl/agent--save-backend next)
      (when-let* ((session (ltl/agent--find-session next)))
        (ltl/agent--display-session session))
      (message "Default agent backend: %s (%s)%s"
               next
               (ltl/agent--executable next)
               (if (ltl/agent--find-session next)
                   ""
                 "; C-c c c to start"))
      next)))

;;;###autoload
(defun ltl/agent-new (&optional arg)
  "Start a new agent session in the agent pane.
Uses the saved default backend (`C-c c a'). Existing sessions stay alive;
switch between them with `C-c c l'.
With prefix ARG:
- plain C-u: quit the current session and replace it
- C-u C-u: prompt for workdir, then start another session
- for Claude YOLO prefer `C-c c !'."
  (interactive "P")
  (let ((backend (ltl/agent--resolved-backend))
        (replace (equal arg '(4)))
        (pick-dir (equal arg '(16))))
    (when pick-dir
      (call-interactively #'ltl/agent-set-directory))
    (ltl/agent--start-session backend
                              ltl/agent--next-skip-permissions
                              (not replace))))

;;;###autoload
(defun ltl/agent (&optional arg)
  "Focus or start a session for the saved default backend.
If a live session of that backend exists, show it. Otherwise start one.
Other backends already running are left alone (`C-c c l' to switch).
With a prefix argument, start a new session like `ltl/agent-new'."
  (interactive "P")
  (ltl/agent--ensure-eat)
  (ltl/agent--prune-sessions)
  (if arg
      (ltl/agent-new arg)
    (let* ((backend (ltl/agent--resolved-backend))
           (session (ltl/agent--find-session backend)))
      (if session
          (ltl/agent--display-session session t)
        (ltl/agent-new)))))

;;;###autoload
(defun ltl/agent-select ()
  "List agent sessions and select one to display."
  (interactive)
  (ltl/agent--prune-sessions)
  (unless ltl/agent--sessions
    (user-error "No agent sessions; start one with C-c c n"))
  (let* ((candidates
          (mapcar (lambda (s)
                    (cons (ltl/agent--session-label s) s))
                  (reverse ltl/agent--sessions)))
         (default (and ltl/agent--current
                       (ltl/agent--session-label ltl/agent--current)))
         (choice (completing-read
                  "Agent session: "
                  (mapcar #'car candidates)
                  nil t nil nil default))
         (session (cdr (assoc choice candidates))))
    (unless session
      (user-error "Unknown session"))
    (ltl/agent--display-session session t)
    (message "Selected %s" choice)))

;;;###autoload
(defun ltl/agent-claude-dangerous ()
  "Start a new Claude Code session with --dangerously-skip-permissions."
  (interactive)
  (unless (ltl/agent--executable 'claude-code)
    (user-error
     "claude is not installed (installed: %s)"
     (ltl/agent--describe-installs)))
  (setq ltl/agent--next-skip-permissions t
        ltl/agent--next-extra-args nil)
  (message "Starting Claude Code with --dangerously-skip-permissions")
  (ltl/agent--start-session 'claude-code t nil))

(defun ltl/agent--send-to-terminal (text &optional session)
  "Send TEXT to SESSION or the current agent process."
  (ltl/agent--prune-sessions)
  (let* ((session (or session ltl/agent--current
                      (cl-find-if #'ltl/agent--session-alive-p
                                  ltl/agent--sessions)))
         (buf (and session (plist-get session :buffer))))
    (unless (and buf (buffer-live-p buf))
      (user-error "No live agent session"))
    (setq ltl/agent--current session)
    (with-current-buffer buf
      (when-let* ((proc (get-buffer-process (current-buffer))))
        (process-send-string proc text)))))

(defun ltl/agent-insert-source-file ()
  "Insert a file reference for the source buffer."
  (interactive)
  (when-let* ((buf ltl/agent--source-buffer)
              (file (buffer-file-name buf)))
    (ltl/agent--send-to-terminal
     (ltl/agent--file-ref file (plist-get ltl/agent--current :backend)))))

(defun ltl/agent-send-file (file)
  "Insert a reference to FILE in the agent terminal."
  (interactive "fFile: ")
  (ltl/agent--send-to-terminal
   (ltl/agent--file-ref file (plist-get ltl/agent--current :backend))))

;;;###autoload
(defun ltl/agent-send-buffer ()
  "Open/focus agent CLI and reference the current buffer file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when file (save-buffer))
    (ltl/agent)
    (when file
      (run-at-time 0.5 nil
                   (lambda ()
                     (ltl/agent--send-to-terminal
                      (ltl/agent--file-ref
                       file
                       (plist-get ltl/agent--current :backend))))))))

;;;###autoload
(defun ltl/agent-send-region (start end)
  "Send region from START to END to the agent CLI via a temp file."
  (interactive "r")
  (let* ((content (buffer-substring-no-properties start end))
         (ext (or (file-name-extension (or (buffer-file-name) (buffer-name)) t) ".txt"))
         (temp-file (make-temp-file "agent-region-" nil ext)))
    (with-temp-file temp-file
      (insert content))
    (ltl/agent)
    (run-at-time 0.5 nil
                 (lambda ()
                   (ltl/agent--send-to-terminal
                    (ltl/agent--file-ref
                     temp-file
                     (plist-get ltl/agent--current :backend)))))))

;;;###autoload
(defun ltl/agent-explain ()
  "Ask the agent to explain the region or buffer."
  (interactive)
  (if (use-region-p)
      (ltl/agent-send-region (region-beginning) (region-end))
    (ltl/agent-send-buffer))
  (run-at-time 1.0 nil
               (lambda ()
                 (ltl/agent--send-to-terminal ltl/agent-prompt-explain))))

;;;###autoload
(defun ltl/agent-review ()
  "Ask the agent to review the current buffer."
  (interactive)
  (ltl/agent-send-buffer)
  (run-at-time 1.0 nil
               (lambda ()
                 (ltl/agent--send-to-terminal ltl/agent-prompt-review))))

;;;###autoload
(defun ltl/agent-fix-error ()
  "Send buffer plus diagnostics to the agent."
  (interactive)
  (let ((errors (ltl/agent--collect-errors)))
    (ltl/agent-send-buffer)
    (run-at-time 1.0 nil
                 (lambda ()
                   (if errors
                       (ltl/agent--send-to-terminal
                        (format ltl/agent-prompt-fix errors))
                     (ltl/agent--send-to-terminal
                      ltl/agent-prompt-fix-general))))))

(defun ltl/agent--collect-errors ()
  "Collect flycheck/flymake diagnostics from the source buffer."
  (with-current-buffer (or ltl/agent--source-buffer (current-buffer))
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
(defun ltl/agent-generate-tests ()
  "Ask the agent to generate tests for the current file."
  (interactive)
  (ltl/agent-send-buffer)
  (run-at-time 1.0 nil
               (lambda ()
                 (ltl/agent--send-to-terminal ltl/agent-prompt-test))))

;;;###autoload
(defun ltl/agent-refactor (instruction)
  "Ask the agent to refactor with INSTRUCTION."
  (interactive "sRefactor instruction: ")
  (ltl/agent-send-buffer)
  (run-at-time 1.0 nil
               (lambda ()
                 (ltl/agent--send-to-terminal
                  (format ltl/agent-prompt-refactor instruction)))))

;;;###autoload
(defun ltl/agent-diff-current-file ()
  "Show diff for the current file against VCS HEAD."
  (interactive)
  (cond
   ((and (buffer-file-name) (fboundp 'magit-diff-buffer-file))
    (magit-diff-buffer-file))
   ((and (buffer-file-name) (fboundp 'vc-diff))
    (vc-diff nil t))
   (t (user-error "No file or VCS diff command available"))))

(defvar ltl/agent--fullscreen-config nil
  "Window configuration saved before entering agent fullscreen.")

;;;###autoload
(defun ltl/agent-fullscreen ()
  "Toggle fullscreen display of the current agent session.
Enter: show only the agent buffer. Exit: restore the previous layout."
  (interactive)
  (ltl/agent--prune-sessions)
  (cond
   (ltl/agent--fullscreen-config
    (set-window-configuration ltl/agent--fullscreen-config)
    (setq ltl/agent--fullscreen-config nil)
    (message "Agent fullscreen off"))
   ((not (ltl/agent--session-alive-p ltl/agent--current))
    (user-error "No live agent session; open one with C-c c c / n / l"))
   (t
    (setq ltl/agent--fullscreen-config (current-window-configuration))
    (let ((buf (plist-get ltl/agent--current :buffer)))
      (delete-other-windows)
      (switch-to-buffer buf)
      (when (fboundp 'eat-semi-char-mode)
        (eat-semi-char-mode)))
    (message "Agent fullscreen on (toggle again with C-c c z)"))))

;;;###autoload
(defun ltl/agent-stop ()
  "Stop the current agent subprocess (keep buffer)."
  (interactive)
  (ltl/agent--prune-sessions)
  (when-let* ((session ltl/agent--current)
              (buf (plist-get session :buffer))
              ((buffer-live-p buf))
              (proc (get-buffer-process buf)))
    (delete-process proc)))

;;;###autoload
(defun ltl/agent-restart ()
  "Restart the current agent session (same backend/root/flags)."
  (interactive)
  (ltl/agent--prune-sessions)
  (let* ((old (or ltl/agent--current
                  (user-error "No agent session to restart")))
         (backend (plist-get old :backend))
         (skip (plist-get old :skip-permissions))
         (root (plist-get old :root)))
    (ltl/agent-quit 'silent)
    (let ((default-directory (or root default-directory)))
      (ltl/agent--start-session backend skip))))

;;;###autoload
(defun ltl/agent-quit (&optional silent)
  "Quit the current agent session only.
If other sessions remain, show the next one in the same pane.
Otherwise close the agent window.
If SILENT is non-nil, keep the window (for replace/restart) and do not
print a message."
  (interactive)
  (ltl/agent--prune-sessions)
  (when-let* ((session (or ltl/agent--current
                           (and (not silent)
                                (user-error "No agent session"))))
              (buf (plist-get session :buffer)))
    (let ((agent-win (and (buffer-live-p buf)
                          (get-buffer-window buf))))
      (when (buffer-live-p buf)
        (when-let* ((proc (get-buffer-process buf)))
          (delete-process proc))
        (with-current-buffer buf
          (remove-hook 'kill-buffer-hook #'ltl/agent--on-buffer-kill t)
          (kill-buffer buf)))
      (setq ltl/agent--sessions (delq session ltl/agent--sessions)
            ltl/agent--current (car ltl/agent--sessions))
      (unless silent
        (cond
         ((and ltl/agent--current
               (buffer-live-p (plist-get ltl/agent--current :buffer)))
          (ltl/agent--display-session ltl/agent--current))
         ((and agent-win
               (window-live-p agent-win)
               (not (one-window-p t)))
          (delete-window agent-win)))
        (message "Quit session. %d remaining." (length ltl/agent--sessions))))))

;;;###autoload
(defun ltl/agent-quit-all ()
  "Quit every agent session."
  (interactive)
  (ltl/agent--prune-sessions)
  (dolist (session (copy-sequence ltl/agent--sessions))
    (when-let* ((buf (plist-get session :buffer))
                ((buffer-live-p buf)))
      (when-let* ((proc (get-buffer-process buf)))
        (delete-process proc))
      (kill-buffer buf)))
  (setq ltl/agent--sessions nil
        ltl/agent--current nil
        ltl/agent--fullscreen-config nil)
  (delete-other-windows)
  (message "All agent sessions closed"))

(with-eval-after-load 'bind-key
  (when (boundp 'ltl/toggles-map)
    (define-key ltl/toggles-map (kbd "C") #'ltl/agent)))

(defvar ltl/agent-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'ltl/agent)
    (define-key map (kbd "n") #'ltl/agent-new)
    (define-key map (kbd "l") #'ltl/agent-select)
    (define-key map (kbd "a") #'ltl/agent-switch)
    (define-key map (kbd "i") #'ltl/agent-status)
    (define-key map (kbd "w") #'ltl/agent-set-directory)
    (define-key map (kbd "!") #'ltl/agent-claude-dangerous)
    (define-key map (kbd "s") #'ltl/agent-send-buffer)
    (define-key map (kbd "r") #'ltl/agent-send-region)
    (define-key map (kbd "e") #'ltl/agent-explain)
    (define-key map (kbd "v") #'ltl/agent-review)
    (define-key map (kbd "f") #'ltl/agent-fix-error)
    (define-key map (kbd "t") #'ltl/agent-generate-tests)
    (define-key map (kbd "R") #'ltl/agent-refactor)
    (define-key map (kbd "d") #'ltl/agent-diff-current-file)
    (define-key map (kbd "z") #'ltl/agent-fullscreen)
    (define-key map (kbd "k") #'ltl/agent-stop)
    (define-key map (kbd "x") #'ltl/agent-restart)
    (define-key map (kbd "q") #'ltl/agent-quit)
    (define-key map (kbd "Q") #'ltl/agent-quit-all)
    map)
  "Keymap for AI agent CLI commands.")

(global-set-key (kbd "C-c c") ltl/agent-command-map)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements ltl/agent-command-map
    "c" "focus/open"
    "n" "new session"
    "l" "list/select session"
    "a" "switch backend"
    "i" "installed agents"
    "w" "set workdir"
    "!" "claude skip-permissions"
    "s" "send buffer"
    "r" "send region"
    "e" "explain"
    "v" "review"
    "f" "fix errors"
    "t" "generate tests"
    "R" "refactor"
    "d" "diff file"
    "z" "fullscreen toggle"
    "k" "stop"
    "x" "restart"
    "q" "quit session"
    "Q" "quit all"))

(ltl/agent--load-backend)

(provide 'init-agent-cli)
;;; init-agent-cli.el ends here
