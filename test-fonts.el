;;; test-fonts.el --- Font installation verification -*- lexical-binding: t -*-

;;; Commentary:
;; Quick script to verify fonts are installed and working

;;; Code:

(defun test-fonts-display ()
  "Display a buffer showing how fonts are rendered."
  (interactive)
  (with-current-buffer (get-buffer-create "*Font Test*")
    (erase-buffer)
    (insert "═══════════════════════════════════════════════════\n")
    (insert "  Font Installation Test\n")
    (insert "═══════════════════════════════════════════════════\n\n")
    
    ;; Current font info
    (insert "Current Font Settings:\n")
    (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    (insert (format "Default font: %s\n" (face-attribute 'default :font)))
    (insert (format "Font height: %s\n\n" (face-attribute 'default :height)))
    
    ;; English characters
    (insert "English (Source Code Pro):\n")
    (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    (insert "ABCDEFGHIJKLMNOPQRSTUVWXYZ\n")
    (insert "abcdefghijklmnopqrstuvwxyz\n")
    (insert "0123456789 !@#$%^&*()-=_+[]{};:'\",.<>?/\n")
    (insert "The quick brown fox jumps over the lazy dog.\n\n")
    
    ;; Chinese characters
    (insert "中文 (Droid Sans Fallback / Source Han Sans):\n")
    (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    (insert "你好世界！这是中文字体测试。\n")
    (insert "配置文件已成功加载和运行。\n")
    (insert "汉字、标点、数字：一二三四五，１２３４５。\n\n")
    
    ;; Mixed content
    (insert "Mixed Content (混合内容):\n")
    (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    (insert "Emacs 配置优化 v3.0\n")
    (insert "LSP Mode 语言服务器\n")
    (insert "Tree-sitter 语法树\n")
    (insert "GitHub Copilot AI 助手\n\n")
    
    ;; Programming example
    (insert "Programming Example:\n")
    (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    (insert ";; Elisp code with Chinese comments\n")
    (insert "(defun hello-world ()\n")
    (insert "  \"打印 Hello World\"  ; 中文注释\n")
    (insert "  (message \"你好，世界！\"))\n\n")
    
    ;; Icons test (if nerd-icons available)
    (when (fboundp 'nerd-icons-faicon)
      (insert "Icons (Nerd Fonts):\n")
      (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
      (insert "       \n\n"))
    
    (insert "═══════════════════════════════════════════════════\n")
    (insert "If you can read all text above clearly, fonts are\n")
    (insert "installed correctly!\n")
    (insert "═══════════════════════════════════════════════════\n")
    
    (goto-char (point-min))
    (special-mode)
    (pop-to-buffer (current-buffer))))

(defun test-fonts-check-installed ()
  "Check if required fonts are installed."
  (interactive)
  (let ((fonts '("Source Code Pro" 
                 "Droid Sans Fallback"
                 "Source Han Sans CN"
                 "SauceCodePro Nerd Font"))
        (results '()))
    (message "\n╔════════════════════════════════════════════════╗")
    (message "║  Font Installation Check                      ║")
    (message "╚════════════════════════════════════════════════╝\n")
    (dolist (font fonts)
      (let ((installed (find-font (font-spec :name font))))
        (push (cons font installed) results)
        (message "%s %s" 
                 (if installed "✓" "✗")
                 font)))
    (message "\n")
    results))

(defun test-fonts-compare-rendering ()
  "Compare text rendering with and without special fonts."
  (interactive)
  (with-current-buffer (get-buffer-create "*Font Comparison*")
    (erase-buffer)
    (let ((test-text "The Quick Brown Fox Jumps Over The Lazy Dog\n你好世界 Hello World 123456"))
      (insert "Font Rendering Comparison\n")
      (insert "══════════════════════════════════════════════\n\n")
      
      ;; Default font
      (insert "Default (Source Code Pro):\n")
      (let ((start (point)))
        (insert test-text)
        (put-text-property start (point) 'face '(:family "Source Code Pro")))
      (insert "\n\n")
      
      ;; Monospace fallback
      (insert "Monospace:\n")
      (let ((start (point)))
        (insert test-text)
        (put-text-property start (point) 'face '(:family "monospace")))
      (insert "\n\n")
      
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;; Quick verification commands
(defun test-fonts-quick-check ()
  "Quick font check with results in minibuffer."
  (interactive)
  (let ((source-code-pro (find-font (font-spec :name "Source Code Pro")))
        (droid-sans (find-font (font-spec :name "Droid Sans Fallback"))))
    (message "Font Check: Source Code Pro [%s] | Droid Sans Fallback [%s]"
             (if source-code-pro "✓" "✗")
             (if droid-sans "✓" "✗"))))

(provide 'test-fonts)
;;; test-fonts.el ends here
