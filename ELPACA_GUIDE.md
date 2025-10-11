# Elpaca 快速参考

Elpaca 是现代的异步 Emacs 包管理器，本配置使用 Elpaca 0.11。

## 📦 包管理命令

### 浏览和搜索包

| 命令 | 快捷键 | 说明 |
|------|--------|------|
| `elpaca-manager` | `M-x elpaca-manager` 或 `g m` | 打开包管理器界面 |
| `elpaca-info` | 在管理器中按 `i` | 查看包信息 |
| `elpaca-try` | `M-x elpaca-try` | 临时试用包（本次会话） |

### 更新包

| 命令 | 快捷键 | 说明 |
|------|--------|------|
| `elpaca-fetch` | 在管理器中按 `f x` | 获取单个包的更新 |
| `elpaca-fetch-all` | `M-x elpaca-fetch-all` | 获取所有包的更新 |
| `elpaca-merge` | 在管理器中按 `m x` | 合并/应用单个包的更新 |
| `elpaca-merge-all` | `M-x elpaca-merge-all` | 合并/应用所有包的更新 |
| `elpaca-update` | 在管理器中按 `p x` | 更新单个包（fetch + merge） |
| `elpaca-update-all` | `M-x elpaca-update-all` | 更新所有包 |

### 包操作

| 命令 | 快捷键 | 说明 |
|------|--------|------|
| `elpaca-rebuild` | 在管理器中按 `r x` | 重新构建包 |
| `elpaca-delete` | 在管理器中按 `d x` | 删除包 |
| `elpaca-visit` | 在管理器中按 `v` | 访问包的源代码仓库 |
| `elpaca-visit` | 在管理器中按 `C-u v` | 访问包的构建目录 |
| `elpaca-browse` | 在管理器中按 `b` | 在浏览器中打开包的网站 |

### 日志和调试

| 命令 | 快捷键 | 说明 |
|------|--------|------|
| `elpaca-log` | 在管理器中按 `g l` | 查看包的构建日志 |
| `elpaca-status` | `M-x elpaca-status` | 查看 Elpaca 状态 |

## 🔧 配置中的用法

### 基本用法

```elisp
;; 安装单个包
(elpaca package-name)

;; 安装并配置包
(elpaca package-name
  (require 'package-name)
  (setq package-option value))
```

### 与 use-package 集成

```elisp
;; 启用 use-package 支持（已在配置中启用）
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(setq elpaca-use-package-by-default t)

;; 使用 use-package（自动通过 Elpaca 安装）
(use-package evil
  :ensure t
  :demand t
  :config
  (evil-mode 1))

;; 禁用 Elpaca（使用内置包）
(use-package emacs
  :ensure nil
  :config
  (setq ring-bell-function #'ignore))

;; 从 GitHub 安装
(use-package some-package
  :ensure (:host github :repo "user/repo"))

;; 等待包安装完成后再继续
(use-package general
  :ensure (:wait t)
  :demand t)
```

## 📚 包配方（Recipes）

### 指定源

```elisp
;; 从 GitHub
(elpaca (package-name :host github :repo "user/repo"))

;; 从 GitLab
(elpaca (package-name :host gitlab :repo "user/repo"))

;; 从 Codeberg
(elpaca (package-name :host codeberg :repo "user/repo"))

;; 指定分支
(elpaca (package-name :host github :repo "user/repo" :branch "develop"))

;; 指定文件
(elpaca (package-name :host github :repo "user/repo" 
                     :files ("*.el" "lisp/*.el")))
```

### 本地开发

```elisp
;; 使用本地路径
(elpaca (package-name :local-repo "~/projects/package-name"))
```

## 🎯 推荐工作流程

### 更新包的最佳实践

1. **获取更新（不应用）：**
   ```
   M-x elpaca-fetch-all
   ```

2. **查看变更：**
   - 在 `elpaca-manager` 中查看哪些包有更新
   - 按 `g l` 查看变更日志

3. **选择性合并：**
   ```
   M-x elpaca-manager
   ;; 光标移到要更新的包上
   ;; 按 m x 合并该包
   ```

4. **或一次性更新所有：**
   ```
   M-x elpaca-merge-all
   ```

### 问题排查

**包安装失败：**
```elisp
;; 1. 查看日志
M-x elpaca-log

;; 2. 重新构建
M-x elpaca-rebuild

;; 3. 删除并重新安装
M-x elpaca-delete
;; 重启 Emacs
```

**Elpaca 本身更新：**
```elisp
;; 1. 打开管理器
M-x elpaca-manager

;; 2. 找到 'elpaca' 包
;; 3. 按 p x 更新
;; 4. 重启 Emacs
```

**完全重置：**
```bash
# 删除所有包
rm -rf ~/.emacs.d/elpaca/

# 重启 Emacs（会重新安装所有包）
emacs
```

## 🔍 管理器界面快捷键

在 `elpaca-manager` 缓冲区中：

| 键 | 功能 |
|----|------|
| `RET` | 查看包详情 |
| `i` | 查看包信息 |
| `v` | 访问包仓库 |
| `b` | 在浏览器中打开 |
| `f x` | 获取更新 |
| `m x` | 合并更新 |
| `p x` | 更新（fetch + merge） |
| `r x` | 重建包 |
| `d x` | 删除包 |
| `g l` | 查看日志 |
| `g r` | 刷新列表 |
| `s` | 搜索 |
| `n` | 下一个 |
| `p` | 上一个 |
| `q` | 退出 |

## 📖 更多信息

- [Elpaca 官方文档](https://github.com/progfolio/elpaca)
- [Elpaca Wiki](https://github.com/progfolio/elpaca/wiki)
- [Elpaca 手册](https://github.com/progfolio/elpaca/blob/master/doc/manual.md)

## 💡 提示

1. **推荐先 fetch 后 merge** 而不是直接 update，这样可以先查看变更
2. **使用 `:wait t`** 对于在 init 文件中需要立即使用的包
3. **Elpaca 异步安装** 意味着包在 `after-init-hook` 后才激活
4. **使用 `elpaca-after-init-hook`** 而不是 `after-init-hook` 来确保所有包已加载
5. **Windows 用户** 如果不能创建符号链接，需要启用 `elpaca-no-symlink-mode`

## 🆚 与 package.el 的区别

| 特性 | Elpaca | package.el |
|------|--------|------------|
| 安装方式 | 异步并行 | 同步阻塞 |
| 包源 | Git 仓库 | 打包的 tar 文件 |
| 开发模式 | 内置支持 | 需要额外配置 |
| UI | 现代化界面 | 基础列表 |
| 性能 | 快速 | 较慢 |
| 依赖管理 | 智能树构建 | 基础支持 |

---

**版本：** Elpaca 0.11  
**最后更新：** 2025-10-11
