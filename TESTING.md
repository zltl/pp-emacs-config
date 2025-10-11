# 配置测试指南

本指南帮助您测试 Emacs 配置的所有改进和优化。

## 📋 测试清单

### 1. 基本启动测试

```bash
# 清理缓存（可选）
cd ~/.emacs.d
rm -rf eln-cache/

# 使用调试模式启动
emacs --debug-init
```

**预期结果：**
- ✅ Emacs 正常启动，无错误
- ✅ 看到模块加载消息：`✓ Loaded init-xxx`
- ✅ 看到成功消息：`✓ Emacs configuration loaded successfully!`
- ✅ 没有错误或警告（在 `*Messages*` 缓冲区）

### 2. 健康检查测试

```elisp
M-x ltl/doctor
```

**预期结果：**
- ✅ 显示详细的健康检查报告
- ✅ 所有关键检查通过（Emacs 版本、Elpaca）
- ✅ 显示缺失的可选组件（如未安装的 LSP 服务器）
- ✅ 总结显示通过的检查数量

### 3. 模块加载测试

检查每个模块是否正确加载：

```elisp
;; 在 *scratch* 缓冲区执行：
(featurep 'init-const)        ; 应该返回 t
(featurep 'init-elpa)         ; 应该返回 t
(featurep 'init-themes)       ; 应该返回 t
(featurep 'init-bindings)     ; 应该返回 t
(featurep 'init-doctor)       ; 应该返回 t
(featurep 'init-applications) ; 应该返回 t
```

**预期结果：**
- ✅ 所有模块都返回 `t`

### 4. 错误处理测试

测试错误处理机制是否正常工作：

```elisp
;; 在 *scratch* 缓冲区测试：
(ltl/safe-require 'non-existent-module)
```

**预期结果：**
- ✅ 返回 `nil`
- ✅ 在 `*Messages*` 中看到错误消息
- ✅ Emacs 继续正常运行，不崩溃

### 5. 应用程序功能测试

#### Dictionary（字典）
```elisp
M-# ; 或 M-x dictionary-lookup-definition
; 输入一个英文单词
```

**预期结果：**
- ✅ 显示单词定义（如果有网络连接）

#### UUID Generation（UUID 生成）
```elisp
M-x uuidgen-4
```

**预期结果：**
- ✅ 在当前光标位置插入 UUID

#### Rime（中文输入）
```elisp
C-\ ; 切换输入法
; 选择 rime
```

**预期结果：**
- ✅ 能够切换到 Rime 输入法（如果安装了 librime）
- 或 ⚠️ 显示警告消息（如果未安装）

### 6. 包管理器测试

#### 查看包列表
```elisp
M-x elpaca-manager
```

**预期结果：**
- ✅ 显示已安装包的列表
- ✅ 界面正常，可以导航

#### 检查更新
```elisp
M-x elpaca-fetch-all
```

**预期结果：**
- ✅ 获取所有包的更新信息
- ✅ 无错误

### 7. 编程功能测试

#### Go 开发
```bash
# 创建测试文件
cat > /tmp/test.go << 'EOF'
package main

func main() {
    println("Hello")
}
EOF

# 在 Emacs 中打开
emacs /tmp/test.go
```

**预期结果：**
- ✅ 语法高亮正常
- ✅ LSP 启动（如果安装了 gopls）
- ✅ 可以使用 `M-.` 跳转定义

#### Tree-sitter 测试
```elisp
M-x treesit-available-p
```

**预期结果：**
- ✅ 返回 `t`（Emacs 29+）

```elisp
;; 检查已安装的语法
(treesit-language-available-p 'go)
(treesit-language-available-p 'python)
```

**预期结果：**
- ✅ 返回 `t`（如果已安装语法）

### 8. Git 集成测试

```elisp
C-x g ; 或 M-x magit-status
```

**预期结果：**
- ✅ 显示 Magit 状态界面
- ✅ 正确显示当前仓库状态

### 9. 补全框架测试

#### Corfu（in-buffer completion）
```elisp
;; 在任何编程文件中开始输入
;; 应该自动显示补全候选
```

**预期结果：**
- ✅ 补全弹窗正常显示
- ✅ 可以用 TAB 或方向键选择

#### Vertico（minibuffer completion）
```elisp
M-x  ; 输入命令
```

**预期结果：**
- ✅ 显示垂直候选列表
- ✅ 可以用方向键或 C-n/C-p 导航

### 10. AI 功能测试（如果已配置）

#### Copilot
```elisp
M-`  ; 触发 Copilot 补全
```

**预期结果：**
- ✅ 显示建议（如果已登录）
- 或 ⚠️ 提示需要登录

### 11. 按键绑定测试

测试新的和移动的按键绑定：

```elisp
C-x C-f  ; 应该调用 find-file-at-point
C-x b    ; 应该调用 switch-to-buffer
C-c j c  ; Avy jump to char
M-x ltl/doctor  ; 健康检查
```

**预期结果：**
- ✅ 所有按键绑定正常工作

### 12. 性能测试

```elisp
M-x benchmark-init/show-durations-tabulated
```

**预期结果：**
- ✅ 显示包加载时间
- ✅ 没有异常慢的包（>2秒）

### 13. 错误日志检查

```elisp
M-x view-echo-area-messages
```

**预期结果：**
- ✅ 没有严重错误消息
- ✅ 只有正常的加载消息和可选的警告

## 🐛 常见问题排查

### 问题 1: 模块加载失败

**症状：** 看到 `✗ Failed to load init-xxx` 消息

**解决方法：**
1. 查看详细错误消息
2. 检查文件是否存在：`ls ~/.emacs.d/lisp/init-xxx.el`
3. 检查文件语法：`M-x check-parens`（在该文件中）
4. 重新加载：`M-x eval-buffer`（在该文件中）

### 问题 2: Elpaca 包安装失败

**症状：** 包无法安装或更新

**解决方法：**
```elisp
;; 查看日志
M-x elpaca-log

;; 重建包
M-x elpaca-rebuild

;; 完全重置（删除并重新安装）
```

```bash
rm -rf ~/.emacs.d/elpaca/
emacs  # 重新启动，会重新安装
```

### 问题 3: LSP 不工作

**症状：** 打开文件时 LSP 没有启动

**解决方法：**
1. 运行健康检查：`M-x ltl/doctor`
2. 检查语言服务器是否安装
3. 查看 LSP 日志：`M-x lsp-workspace-show-log`
4. 重启 LSP：`M-x lsp-workspace-restart`

### 问题 4: 字体显示异常

**症状：** 中文或图标无法正常显示

**解决方法：**
1. 运行健康检查：`M-x ltl/doctor`
2. 确认字体已安装
3. 重启 Emacs
4. 安装图标字体：`M-x all-the-icons-install-fonts`

### 问题 5: Tree-sitter 语法缺失

**症状：** 语法高亮不工作或 doctor 报告语法缺失

**解决方法：**
```elisp
M-x mp-setup-install-grammars
```

## ✅ 测试完成清单

在提交更改前，确保以下所有项目都已测试：

- [ ] Emacs 正常启动，无错误
- [ ] `ltl/doctor` 运行正常，显示健康报告
- [ ] 所有模块正确加载（使用 `featurep` 测试）
- [ ] 错误处理机制工作正常
- [ ] 应用程序功能（dictionary, uuidgen, rime）可用
- [ ] Elpaca 包管理器正常工作
- [ ] 编程功能（LSP, tree-sitter）正常
- [ ] Git 集成（Magit）正常
- [ ] 补全框架（Corfu, Vertico）正常
- [ ] 按键绑定正确
- [ ] 性能可接受（启动时间 <10秒）
- [ ] 无严重错误或警告

## 📊 性能基准

**正常启动时间（参考值）：**
- 首次启动（安装包）：2-3 分钟
- 正常启动：3-8 秒
- 使用 daemon 模式：< 1 秒

**内存使用（参考值）：**
- 启动后：100-200 MB
- 打开多个文件后：200-400 MB
- 使用 LSP 后：300-600 MB

## 🔄 回滚指令

如果测试发现严重问题，可以回滚到之前的版本：

```bash
cd ~/.emacs.d
git log --oneline  # 查看提交历史
git checkout <previous-commit-hash>  # 回滚到特定提交
# 或
git reset --hard HEAD~1  # 回滚一个提交

# 重启 Emacs
emacs
```

## 📝 报告问题

如果发现问题，请记录：
1. Emacs 版本：`M-x emacs-version`
2. 操作系统和版本
3. 错误消息（来自 `*Messages*` 或 `*Warnings*`）
4. 重现步骤
5. `ltl/doctor` 输出

---

**最后更新：** 2025-10-11  
**测试版本：** 优化版 v2.0
