# 配置检查清单

使用此清单确保您的 Emacs 配置正确设置并可以使用。

---

## 安装检查

### 基础要求
- [ ] **Emacs 版本** >= 30  
  ```bash
  emacs --version
  ```
  
- [ ] **Git 已安装**  
  ```bash
  git --version
  ```

- [ ] **配置已克隆**  
  ```bash
  ls ~/.emacs.d/init.el
  ```

### 可选工具（推荐）
- [ ] **ag (The Silver Searcher)**  
  ```bash
  ag --version
  ```
  
- [ ] **fd (文件查找)**  
  ```bash
  fd --version
  ```
  
- [ ] **Node.js** (Copilot 需要)  
  ```bash
  node --version
  ```
  
- [ ] **ripgrep** (快速搜索)  
  ```bash
  rg --version
  ```

---

## 字体检查

### 必需字体
- [ ] **Source Code Pro**  
  检查：在 Emacs 中 `M-x describe-font RET Source Code Pro`
  
- [ ] **Droid Sans Fallback**  
  检查：在 Emacs 中 `M-x describe-font RET Droid Sans Fallback`

### 图标字体
- [ ] **all-the-icons 已安装**  
  ```elisp
  M-x all-the-icons-install-fonts
  ```
  确认并重启 Emacs

---

## 语言服务器检查

根据你使用的语言，安装相应的 LSP 服务器：

### Go
- [ ] **gopls 已安装**  
  ```bash
  gopls version
  ```
  安装：`go install golang.org/x/tools/gopls@latest`

### C/C++
- [ ] **clangd 已安装**  
  ```bash
  clangd --version
  ```
  安装：参考 https://clangd.llvm.org/installation.html

### Rust
- [ ] **rust-analyzer 已安装**  
  ```bash
  rust-analyzer --version
  ```
  安装：`rustup component add rust-analyzer`

### Python
- [ ] **pyright 已安装**  
  ```bash
  pyright --version
  ```
  安装：`pip install pyright` 或 `npm install -g pyright`

### TypeScript/JavaScript
- [ ] **typescript-language-server 已安装**  
  ```bash
  typescript-language-server --version
  ```
  安装：`npm install -g typescript-language-server typescript`

### 其他语言
根据需要安装相应的语言服务器。

---

## 启动检查

### 首次启动
- [ ] **Emacs 正常启动**  
  ```bash
  emacs
  ```
  等待包安装完成（可能需要几分钟）

- [ ] **没有错误消息**  
  检查 `*Messages*` buffer (C-h e 或 M-x view-echo-area-messages)

- [ ] **包安装完成**  
  等到看到 "✓ Emacs configuration loaded successfully!"

### 调试启动（如有问题）
- [ ] **使用调试模式**  
  ```bash
  emacs --debug-init
  ```
  查看详细错误信息

---

## 健康检查

- [ ] **运行诊断工具**  
  ```elisp
  M-x ltl/doctor
  ```

- [ ] **检查输出**  
  确保关键项都有 ✓ 标记：
  - ✓ Emacs 版本
  - ✓ 目录结构
  - ✓ Elpaca 状态
  - ✓ 必需程序
  - 可选项可以忽略

---

## 功能测试

### 基础功能
- [ ] **文件打开** (C-x C-f)
- [ ] **缓冲区切换** (C-x b)
- [ ] **保存文件** (C-x C-s)
- [ ] **搜索** (C-s)
- [ ] **撤销** (C-/)

### 补全系统
- [ ] **Vertico 补全工作**  
  尝试 C-x C-f 看是否有补全列表

- [ ] **Corfu 代码补全工作**  
  打开代码文件，输入时应该有补全提示

### 编程功能
- [ ] **打开代码文件**  
  测试 .go, .py, .rs 或你常用的文件类型

- [ ] **语法高亮正常**  
  代码应该有颜色

- [ ] **LSP 工作** (如已安装服务器)  
  ```elisp
  M-x lsp
  ```
  在代码文件中启动，应该看到 LSP 连接成功

- [ ] **跳转到定义** (M-.)  
  光标放在函数名上，按 M-. 应该跳转

- [ ] **查找引用** (M-?)  
  应该列出所有引用位置

### Git 集成
- [ ] **Magit 工作**  
  ```elisp
  M-x magit-status  或 C-x g
  ```
  (需要在 Git 仓库中)

### Copilot (如启用)
- [ ] **Copilot 登录**  
  ```elisp
  M-x copilot-login
  ```
  按提示完成认证

- [ ] **Copilot 建议**  
  在代码文件中输入注释，应该看到灰色建议

---

## 自定义配置

- [ ] **打开自定义界面**  
  ```elisp
  M-x customize-group RET ltl RET
  ```

- [ ] **浏览可用选项**  
  查看所有可配置的变量

- [ ] **测试修改**  
  修改一个设置（如字体），看是否生效

- [ ] **保存自定义**  
  在 customize 界面点击 "Save for future sessions"

---

## 文档阅读

- [ ] **阅读 README.md**  
  了解配置概览

- [ ] **阅读 QUICK_REFERENCE.md**  
  熟悉常用命令

- [ ] **浏览 TESTING.md**  
  了解如何测试各个功能

- [ ] **查看 ELPACA_GUIDE.md**  
  包管理器使用指南

- [ ] **查看 FONT_INSTALLATION.md**  
  字体安装指南

---

## 性能验证

- [ ] **检查启动时间**  
  ```elisp
  M-x benchmark-init/show-durations-tabulated
  ```
  启动时间应该在 2-5 秒左右

- [ ] **检查内存使用**  
  ```elisp
  M-x memory-report
  ```
  启动后内存应该在 80-150 MB

- [ ] **没有明显卡顿**  
  正常编辑应该流畅

---

## 更新测试

- [ ] **测试包更新**  
  ```elisp
  M-x elpaca-fetch-all
  ```
  应该能获取更新列表

- [ ] **测试包管理器界面**  
  ```elisp
  M-x elpaca-manager
  ```
  应该显示包列表

---

## 常见使用场景测试

### 场景 1: 编写 Go 代码
- [ ] 打开 .go 文件
- [ ] LSP 自动启动
- [ ] 代码补全工作
- [ ] 跳转定义工作
- [ ] 保存时格式化（如配置）

### 场景 2: 使用 Git
- [ ] `C-x g` 打开 Magit
- [ ] 查看文件改动
- [ ] Stage 和 commit
- [ ] 查看日志

### 场景 3: 搜索代码
- [ ] `M-s g` ripgrep 搜索
- [ ] 结果正确显示
- [ ] 可以跳转到匹配位置

### 场景 4: 项目浏览
- [ ] `M-x projectile-find-file` (C-c p f)
- [ ] `M-x projectile-switch-project` (C-c p p)
- [ ] 项目文件正确列出

---

## 最终确认

- [ ] **所有关键功能正常**
- [ ] **没有严重错误或警告**
- [ ] **启动时间可接受**
- [ ] **已阅读主要文档**
- [ ] **知道如何查找帮助**

---

## 如果遇到问题

### 1. 查看错误信息
```elisp
M-x view-echo-area-messages  或 C-h e
```

### 2. 运行健康检查
```elisp
M-x ltl/doctor
```

### 3. 查看日志
```elisp
M-x elpaca-log
```

### 4. 搜索文档
- [README.md](README.md)
- [TESTING.md](TESTING.md)
- [QUICK_REFERENCE.md](QUICK_REFERENCE.md)
- [ELPACA_GUIDE.md](ELPACA_GUIDE.md)
- [FONT_INSTALLATION.md](FONT_INSTALLATION.md)

### 5. 重置配置（最后手段）
```bash
cd ~/.emacs.d
git reset --hard origin/main
git clean -fd
```

---

## 完成！

如果所有项都已勾选，恭喜！您的 Emacs 配置已经完全设置好了！

开始享受您的 Emacs 之旅吧！

---

**记得定期更新：**
```bash
cd ~/.emacs.d
git pull
```
```elisp
M-x elpaca-fetch-all
M-x elpaca-merge-all
```
