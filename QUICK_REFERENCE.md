# 快速参考卡片

## 常用命令速查

### 配置诊断
```
M-x ltl/doctor                    运行完整健康检查
M-x describe-variable ltl/       查看所有 ltl 变量
M-x customize-group ltl           打开自定义界面
M-x ltl/apply-custom-settings     应用自定义设置
```

### 包管理（Elpaca）
```
M-x elpaca-manager               包管理器界面
M-x elpaca-fetch-all             获取所有更新
M-x elpaca-merge-all             应用所有更新
M-x elpaca-try                   临时试用包
M-x elpaca-rebuild               重新构建包
M-x elpaca-delete                删除包
M-x elpaca-log                   查看日志
```

### 代码导航（Eglot）
```
M-.         跳转到定义        xref-find-definitions
M-,         返回              xref-pop-marker-stack
M-?         查找引用          xref-find-references
C-c l r     重命名符号        eglot-rename
C-c l f     格式化文档        eglot-format
C-c l a     代码操作          eglot-code-actions
C-c l d     查看文档          eldoc-doc-buffer
C-c l s     搜索符号          consult-eglot-symbols
```

### Tree-sitter
```
M-x treesit-install-language-grammar    安装语法
M-x treesit-explore-mode               探索语法树
M-x combobulate-mode                   启用结构编辑
```

### 文件管理
```
C-x C-f     打开文件          find-file
C-x b       切换缓冲区        switch-to-buffer
C-x C-s     保存              save-buffer
C-x s       保存所有          save-some-buffers
C-x k       关闭缓冲区        kill-buffer
```

### 搜索（Vertico/Consult）
```
C-s         向前搜索          isearch-forward
C-r         向后搜索          isearch-backward
M-s g       Grep 搜索         consult-ripgrep
M-s l       跳转到行          consult-goto-line
M-s o       查找             consult-outline
M-s i       Imenu            consult-imenu
```

### Git（Magit）
```
C-x g       Magit 状态        magit-status
C-c g b     Blame            magit-blame
C-c g l     日志             magit-log-current
C-c g f     文件日志          magit-log-buffer-file
C-c g t     时间机器          git-timemachine
```

### Copilot
```
C-c M-c     启用/禁用         copilot-mode
M-\         接受建议          copilot-accept-completion
M-n         下一个建议        copilot-next-completion
M-p         上一个建议        copilot-previous-completion
C-g         拒绝建议          keyboard-quit
```

### Shell/Terminal
```
M-x eshell                    启动 Eshell
M-x shell                     启动 Shell
M-x vterm                     启动 Vterm（如果安装）
C-c C-c                       中断进程
C-c C-k                       杀死进程
```

### 性能分析
```
M-x benchmark-init/show-durations-tabulated    查看启动时间
M-x profiler-start                             开始性能分析
M-x profiler-report                            查看报告
M-x profiler-stop                              停止分析
```

---

## 重要变量

### UI 设置
```elisp
ltl/default-font              "Source Code Pro"
ltl/font-size                 110
ltl/chinese-font              "Droid Sans Fallback"
ltl/theme                     'spacemacs-dark
```

### 功能开关
```elisp
ltl/enable-copilot            t
ltl/enable-lsp                t      ; 控制 Eglot
ltl/enable-tree-sitter        t
ltl/enable-dashboard          t
```

### 编程设置
```elisp
ltl/indent-level              4
ltl/web-indent-level          2
ltl/use-tabs                  nil
ltl/show-trailing-whitespace  t
```

### Eglot/LSP 设置
```elisp
ltl/lsp-language-servers      '((go . "gopls")
                                (c . "clangd")
                                (python . "pyright")
                                (rust . "rust-analyzer"))
```

### 性能设置
```elisp
ltl/gc-cons-threshold         (* 100 1024 1024)  ; 100 MB
ltl/read-process-output-max   (* 1024 1024)      ; 1 MB
```

---

## 修改配置

### 方法 1: Customize 界面（推荐）
```
M-x customize-group RET ltl RET
```
- 图形化界面
- 立即预览
- 自动保存

### 方法 2: 直接编辑 custom.el
```elisp
;; ~/.emacs.d/custom.el
(setq ltl/default-font "JetBrains Mono")
(setq ltl/font-size 120)
(setq ltl/theme 'doom-one)
```

### 方法 3: 临时测试
```elisp
;; 在 *scratch* buffer 中
(setq ltl/enable-copilot nil)
M-x ltl/apply-custom-settings
```

---

## 紧急修复

### Emacs 无法启动
```bash
# 调试模式启动
emacs --debug-init

# 安全模式（不加载配置）
emacs -Q

# 检查配置
cd ~/.emacs.d
git status
git diff
```

### 包管理问题
```elisp
M-x elpaca-log                   ; 查看日志
M-x elpaca-status                ; 查看状态
M-x elpaca-rebuild <package>     ; 重建包
```

### LSP 不工作
```bash
# 检查服务器
M-x ltl/doctor

# 重启 Eglot
M-x eglot-reconnect

# 查看日志
M-x eglot-events-buffer
```

### 性能问题
```elisp
;; 查看启动耗时
M-x benchmark-init/show-durations-tabulated

;; 增加 GC 阈值
(setq ltl/gc-cons-threshold (* 200 1024 1024))
M-x ltl/apply-custom-settings
```

---

## 文档导航

| 文档 | 用途 |
|------|------|
| [README.md](README.md) | 主文档 |
| [CHECKLIST.md](CHECKLIST.md) | 设置清单 |
| [QUICK_REFERENCE.md](QUICK_REFERENCE.md) | 快速参考 |
| [TESTING.md](TESTING.md) | 测试指南 |
| [ELPACA_GUIDE.md](ELPACA_GUIDE.md) | 包管理 |
| [FONT_INSTALLATION.md](FONT_INSTALLATION.md) | 字体安装 |

---

## 常见场景

### 场景 1: 新机器设置
```bash
cd ~
git clone https://github.com/zltl/pp-emacs-config .emacs.d
emacs  # 等待包安装
M-x ltl/doctor  # 检查健康
```

### 场景 2: 更新配置
```bash
cd ~/.emacs.d
git pull
```
```elisp
M-x elpaca-fetch-all
M-x elpaca-merge-all
```

### 场景 3: 安装新语言支持
```bash
# 安装语言服务器（以 Go 为例）
go install golang.org/x/tools/gopls@latest
```
```elisp
M-x ltl/doctor  # 验证安装
```

### 场景 4: 性能优化
```elisp
;; 查看启动耗时
M-x benchmark-init/show-durations-tabulated

;; 调整设置
M-x customize-group RET ltl RET
;; 修改 ltl/gc-cons-threshold 和 ltl/read-process-output-max

;; 应用
M-x ltl/apply-custom-settings
```

---

## 快速技巧

1. **使用 which-key** - 按任何前缀键（如 C-c）然后等待，会显示可用命令

2. **补全无处不在** - Vertico 和 Corfu 提供智能补全

3. **Embark 快捷操作** - 在 minibuffer 中按 `C-;` 查看可用操作

4. **Magit 是神器** - `C-x g` 打开后，按 `?` 查看所有命令

5. **Eglot 悬停文档** - 光标停在符号上，按 `C-c l d` 查看文档

6. **Treemacs 项目浏览** - `M-x treemacs` 打开项目树

---

**打印此页作为参考！Happy Hacking!**
