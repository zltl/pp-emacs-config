;;; install-fonts.el --- Automatically download and install required fonts -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This script automatically downloads and installs fonts required by the Emacs configuration:
;; - Source Code Pro (programming font)
;; - Droid Sans Fallback (CJK fallback)
;; - Source Han Sans (Chinese font)
;; - Nerd Fonts (icons)
;;
;; Usage:
;;   emacs --batch -l install-fonts.el
;;   or
;;   M-x load-file RET install-fonts.el RET
;;
;;; Code:

(require 'url)
(require 'url-http)

;;; Configuration

(defvar font-install-dir
  (expand-file-name
   (cond
    ((eq system-type 'gnu/linux) "~/.local/share/fonts")
    ((eq system-type 'darwin) "~/Library/Fonts")
    ((eq system-type 'windows-nt) (concat (getenv "LOCALAPPDATA") "/Microsoft/Windows/Fonts"))
    (t "~/.fonts")))
  "Directory where fonts will be installed.")

(defvar font-download-dir
  (expand-file-name "~/.emacs.d/fonts-download")
  "Temporary directory for downloading fonts.")

;;; Font definitions

(defvar required-fonts
  '(
    ;; Source Code Pro
    (:name "Source Code Pro"
     :url "https://github.com/adobe-fonts/source-code-pro/releases/download/2.042R-u%2F1.062R-i%2F1.026R-vf/OTF-source-code-pro-2.042R-u_1.062R-i.zip"
     :type :zip
     :files nil)  ; Install all fonts found
    
    ;; Nerd Fonts - Source Code Pro variant
    ;; Note: Use latest stable release from https://github.com/ryanoasis/nerd-fonts/releases
    (:name "Source Code Pro Nerd Font"
     :url "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/SourceCodePro.zip"
     :type :zip
     :files nil)  ; Install all fonts found
    
    ;; Droid Sans Fallback (direct file)
    (:name "Droid Sans Fallback"
     :url "https://github.com/LibreOffice/core/raw/master/extras/source/truetype/symbol/DroidSansFallback.ttf"
     :type :direct
     :filename "DroidSansFallback.ttf")
    
    ;; Source Han Sans CN (Simplified Chinese)
    (:name "Source Han Sans CN"
     :url "https://github.com/adobe-fonts/source-han-sans/releases/download/2.004R/SourceHanSansCN.zip"
     :type :zip
     :files nil))  ; Install all fonts found
  "List of fonts to download and install.")

;;; Helper functions

(defun font-install--message (format-string &rest args)
  "Print a message with FORMAT-STRING and ARGS."
  (let ((msg (apply #'format format-string args)))
    (if noninteractive
        (message "%s" msg)
      (message "%s" msg))
    msg))

(defun font-install--ensure-directory (dir)
  "Ensure directory DIR exists."
  (unless (file-exists-p dir)
    (make-directory dir t)
    (font-install--message "✓ Created directory: %s" dir)))

(defun font-install--download-file (url dest)
  "Download file from URL to DEST."
  (font-install--message "  Downloading: %s" (file-name-nondirectory dest))
  (url-copy-file url dest t)
  (font-install--message "  ✓ Downloaded to: %s" dest))

(defun font-install--unzip-file (zip-file dest-dir)
  "Extract ZIP-FILE to DEST-DIR."
  (font-install--message "  Extracting: %s" (file-name-nondirectory zip-file))
  ;; Ensure destination directory exists before extracting
  (font-install--ensure-directory dest-dir)
  (cond
   ;; Use unzip command if available
   ((executable-find "unzip")
    (shell-command (format "unzip -o -q '%s' -d '%s'" zip-file dest-dir)))
   ;; Use 7z if available
   ((executable-find "7z")
    (shell-command (format "7z x -y -o'%s' '%s'" dest-dir zip-file)))
   ;; Use PowerShell on Windows
   ((and (eq system-type 'windows-nt) (executable-find "powershell"))
    (shell-command (format "powershell -command \"Expand-Archive -Force '%s' '%s'\"" 
                           zip-file dest-dir)))
   (t
    (error "No unzip utility found. Please install unzip, 7z, or use PowerShell on Windows"))))

(defun font-install--copy-fonts (source-dir patterns dest-dir)
  "Copy font files matching PATTERNS from SOURCE-DIR to DEST-DIR.
If PATTERNS is nil or empty, copy all font files (*.ttf, *.otf, *.woff, *.woff2)."
  (let ((copied 0)
        (actual-patterns (if (and patterns (listp patterns) (> (length patterns) 0))
                            patterns
                          ;; Default: all common font formats
                          '("\\.ttf\\'" "\\.otf\\'" "\\.woff\\'" "\\.woff2\\'"))))
    (dolist (pattern actual-patterns)
      (let ((files (directory-files-recursively source-dir pattern t)))
        (dolist (file files)
          (when (and (file-regular-p file)
                     ;; Skip hidden files and known non-font files
                     (not (string-match-p "/\\." (file-name-nondirectory file)))
                     (not (string-match-p "README\\|LICENSE\\|OFL" file)))
            (let* ((basename (file-name-nondirectory file))
                   (dest-file (expand-file-name basename dest-dir)))
              (copy-file file dest-file t)
              (setq copied (1+ copied))
              (font-install--message "    ✓ Installed: %s" basename))))))
    copied))

(defun font-install--font-installed-p (font-name)
  "Check if FONT-NAME is already installed."
  (when (display-graphic-p)
    (if (find-font (font-spec :name font-name))
        t
      nil)))

(defun font-install--refresh-font-cache ()
  "Refresh system font cache."
  (font-install--message "Refreshing font cache...")
  (cond
   ((eq system-type 'gnu/linux)
    (when (executable-find "fc-cache")
      (shell-command "fc-cache -f -v")
      (font-install--message "✓ Font cache refreshed")))
   ((eq system-type 'darwin)
    (font-install--message "✓ macOS will refresh fonts automatically"))
   ((eq system-type 'windows-nt)
    (font-install--message "✓ Windows will refresh fonts automatically"))))

;;; Main installation function

(defun font-install--install-font (font-spec)
  "Install a font according to FONT-SPEC."
  (let* ((name (plist-get font-spec :name))
         (url (plist-get font-spec :url))
         (type (plist-get font-spec :type))
         (files (plist-get font-spec :files))
         (filename (or (plist-get font-spec :filename)
                      (file-name-nondirectory url)))
         (download-path (expand-file-name filename font-download-dir)))
    
    (font-install--message "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
    (font-install--message "Installing: %s" name)
    (font-install--message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
    
    ;; Download
    (condition-case err
        (progn
          (font-install--download-file url download-path)
          
          ;; Install based on type
          (cond
           ((eq type :direct)
            ;; Direct font file
            (let ((dest-file (expand-file-name filename font-install-dir)))
              (copy-file download-path dest-file t)
              (font-install--message "  ✓ Installed: %s" filename)))
           
           ((eq type :zip)
            ;; ZIP archive
            (let ((extract-dir (expand-file-name 
                               (file-name-sans-extension filename)
                               font-download-dir)))
              (font-install--unzip-file download-path extract-dir)
              (let ((copied (font-install--copy-fonts extract-dir files font-install-dir)))
                (font-install--message "  ✓ Installed %d font files" copied))))
           
           (t
            (error "Unknown font type: %s" type)))
          
          (font-install--message "✓ %s installed successfully\n" name))
      
      (error
       (font-install--message "✗ Failed to install %s: %s\n" name (error-message-string err))))))

;;;###autoload
(defun install-required-fonts ()
  "Download and install all required fonts."
  (interactive)
  
  (font-install--message "\n")
  (font-install--message "═════════════════════════════════════════════════════")
  (font-install--message "  Emacs Font Installation Script")
  (font-install--message "═════════════════════════════════════════════════════")
  (font-install--message "")
  
  ;; Ensure directories exist
  (font-install--ensure-directory font-install-dir)
  (font-install--ensure-directory font-download-dir)
  
  ;; Install each font
  (let ((success 0)
        (failed 0))
    (dolist (font required-fonts)
      (condition-case err
          (progn
            (font-install--install-font font)
            (setq success (1+ success)))
        (error
         (setq failed (1+ failed))
         (font-install--message "Error installing font: %s" (error-message-string err)))))
    
    ;; Refresh font cache
    (font-install--message "\n")
    (font-install--refresh-font-cache)
    
    ;; Summary
    (font-install--message "\n")
    (font-install--message "═════════════════════════════════════════════════════")
    (font-install--message "  Installation Summary")
    (font-install--message "═════════════════════════════════════════════════════")
    (font-install--message "")
    (font-install--message "Fonts installed: %d" success)
    (font-install--message "Failed: %d" failed)
    (font-install--message "Install directory: %s" font-install-dir)
    (font-install--message "")
    
    (if (> failed 0)
        (font-install--message "⚠ Some fonts failed to install. Please check errors above.")
      (font-install--message "✓ All fonts installed successfully!"))
    
    (font-install--message "")
    (font-install--message "Please restart Emacs to use the new fonts.")
    (font-install--message "═════════════════════════════════════════════════════")
    (font-install--message ""))
  
  ;; Clean up download directory
  (when (yes-or-no-p "Clean up downloaded files? ")
    (delete-directory font-download-dir t)
    (font-install--message "✓ Cleaned up download directory")))

;;;###autoload
(defun check-required-fonts ()
  "Check which required fonts are already installed."
  (interactive)
  
  (font-install--message "\n")
  (font-install--message "═════════════════════════════════════════════════════")
  (font-install--message "  Font Installation Status")
  (font-install--message "═════════════════════════════════════════════════════")
  (font-install--message "")
  
  (let ((fonts-to-check '("Source Code Pro"
                          "SauceCodePro Nerd Font"
                          "Droid Sans Fallback"
                          "Source Han Sans CN")))
    (dolist (font fonts-to-check)
      (if (font-install--font-installed-p font)
          (font-install--message "✓ Installed: %s" font)
        (font-install--message "✗ Missing:   %s" font))))
  
  (font-install--message "")
  (font-install--message "Run M-x install-required-fonts to install missing fonts")
  (font-install--message "═════════════════════════════════════════════════════")
  (font-install--message ""))

;; When run in batch mode, automatically start installation
(when noninteractive
  (install-required-fonts))

(provide 'install-fonts)

;;; install-fonts.el ends here
