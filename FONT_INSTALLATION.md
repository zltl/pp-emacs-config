# 字体安装指南

本配置需要以下字体才能获得最佳视觉效果：

## 📦 必需字体

1. **Source Code Pro** - 主要编程字体
2. **Source Code Pro Nerd Font** - 带图标的编程字体
3. **Droid Sans Fallback** - CJK 字符后备字体
4. **Source Han Sans CN** - 中文字体（思源黑体）

## 🚀 自动安装

### 方法 1: 使用 Elisp 脚本（推荐）

在 Emacs 中执行：

```elisp
M-x load-file RET ~/.emacs.d/install-fonts.el RET
M-x install-required-fonts RET
```

或者在命令行中：

```bash
emacs --batch -l ~/.emacs.d/install-fonts.el
```

### 方法 2: 使用 Shell 脚本

```bash
cd ~/.emacs.d
./install-fonts.sh
```

## 🔍 检查字体安装状态

### 在 Emacs 中检查

```elisp
M-x load-file RET ~/.emacs.d/install-fonts.el RET
M-x check-required-fonts RET
```

### 在终端中检查

```bash
# Linux
fc-list | grep -i "source code pro"
fc-list | grep -i "droid sans"
fc-list | grep -i "source han sans"

# macOS
system_profiler SPFontsDataType | grep -i "source code pro"

# Windows (PowerShell)
[System.Reflection.Assembly]::LoadWithPartialName("System.Drawing")
(New-Object System.Drawing.Text.InstalledFontCollection).Families | Where-Object {$_.Name -like "*Source*"}
```

## 📋 手动安装

如果自动安装失败，可以手动下载并安装：

### Source Code Pro

1. 访问：https://github.com/adobe-fonts/source-code-pro/releases
2. 下载最新的 OTF 或 TTF 版本
3. 安装所有 `.otf` 或 `.ttf` 文件

### Source Code Pro Nerd Font

1. 访问：https://www.nerdfonts.com/font-downloads
2. 搜索 "Source Code Pro"
3. 下载并安装

### Droid Sans Fallback

**Linux:**
```bash
# Gentoo
sudo emerge --ask media-fonts/droid

# Ubuntu/Debian
sudo apt install fonts-droid-fallback

# Fedora
sudo dnf install google-droid-sans-fonts

# Arch
sudo pacman -S ttf-droid
```

**macOS/Windows:**
1. 下载：https://github.com/LibreOffice/core/raw/master/extras/source/truetype/symbol/DroidSansFallback.ttf
2. 双击安装

### Source Han Sans CN（思源黑体）

1. 访问：https://github.com/adobe-fonts/source-han-sans/releases
2. 下载 `SourceHanSansCN.zip`
3. 解压并安装所有 `.otf` 文件

## 🖥️ 平台特定说明

### Linux

**字体目录：**
- 用户字体：`~/.local/share/fonts/`
- 系统字体：`/usr/share/fonts/`

**刷新字体缓存：**
```bash
fc-cache -f -v
```

**验证安装：**
```bash
fc-list : family | grep -i "source code pro"
```

### macOS

**字体目录：**
- 用户字体：`~/Library/Fonts/`
- 系统字体：`/Library/Fonts/`

**安装方法：**
- 双击字体文件
- 使用 Font Book 应用

**验证安装：**
```bash
system_profiler SPFontsDataType | grep -i "source code pro"
```

### Windows

**字体目录：**
- 用户字体：`%LOCALAPPDATA%\Microsoft\Windows\Fonts`
- 系统字体：`C:\Windows\Fonts`

**安装方法：**
- 右键点击字体文件 → "安装" 或 "为所有用户安装"
- 拖拽到 `C:\Windows\Fonts` 目录

**验证安装：**
```powershell
[System.Reflection.Assembly]::LoadWithPartialName("System.Drawing")
(New-Object System.Drawing.Text.InstalledFontCollection).Families | Where-Object {$_.Name -like "*Source Code Pro*"}
```

## ⚙️ 配置说明

字体配置位于 `lisp/ui/init-themes.el`：

```elisp
;; 设置字体
(when (display-graphic-p)
  (let ((font-size 110))
    ;; 主字体：Source Code Pro
    (set-face-attribute 'default nil 
                        :font "Source Code Pro" 
                        :weight 'normal 
                        :height font-size)
    
    ;; 中文字体：Droid Sans Fallback
    (set-fontset-font t 'han 
                      (font-spec :family "Droid Sans Fallback" 
                                :weight 'normal 
                                :height font-size))
    
    ;; ASCII 字体
    (set-fontset-font t 'ascii 
                      (font-spec :family "Source Code Pro" 
                                :weight 'normal 
                                :slant 'normal 
                                :height font-size))))
```

### 自定义字体大小

在 `init-themes.el` 中修改 `font-size` 变量：

```elisp
(let ((font-size 120))  ; 改为你想要的大小（110 = 11pt）
  ...)
```

或者使用快捷键动态调整：
- `C-x C-=` - 放大
- `C-x C--` - 缩小
- `C-x C-0` - 重置

## 🔧 图标字体

配置还使用了以下图标字体（自动安装）：

- **all-the-icons** - 各种图标
- **nerd-icons** - Nerd Fonts 图标

这些图标字体在首次使用时会自动安装：

```elisp
M-x all-the-icons-install-fonts
M-x nerd-icons-install-fonts
```

## ❓ 常见问题

### Q: 字体安装后 Emacs 中看不到？

**A:** 尝试以下步骤：
1. 重启 Emacs
2. 刷新字体缓存（Linux: `fc-cache -f -v`）
3. 注销并重新登录系统
4. 在 Emacs 中检查：`M-: (font-family-list) RET`

### Q: 中文显示为方块？

**A:** 
1. 确保安装了 Droid Sans Fallback 或 Source Han Sans CN
2. 检查字体配置中的 `han` fontset
3. 尝试手动设置：
```elisp
M-: (set-fontset-font t 'han (font-spec :family "Droid Sans Fallback"))
```

### Q: 图标显示为方块或问号？

**A:**
1. 运行 `M-x all-the-icons-install-fonts`
2. 运行 `M-x nerd-icons-install-fonts`
3. 重启 Emacs

### Q: 终端中字体不生效？

**A:** 这些字体配置仅在图形界面（GUI）模式下生效。终端模式下字体由终端模拟器控制。

### Q: 字体下载失败？

**A:**
1. 检查网络连接
2. 尝试使用代理或 VPN
3. 手动下载字体文件
4. 检查是否有防火墙阻止

### Q: 权限错误？

**A:**
- Linux/macOS: 使用用户字体目录 `~/.local/share/fonts/` 或 `~/Library/Fonts/`
- 不需要 sudo 权限
- 如果需要系统级安装，使用 `sudo`

## 📚 脚本功能

### install-fonts.el

**主要功能：**
- `install-required-fonts` - 安装所有必需字体
- `check-required-fonts` - 检查字体安装状态
- `font-install--font-installed-p` - 检查单个字体

**特点：**
- ✅ 跨平台支持（Linux, macOS, Windows）
- ✅ 自动检测字体目录
- ✅ 下载进度显示
- ✅ 错误处理
- ✅ 批处理模式支持

### install-fonts.sh

**特点：**
- ✅ 彩色输出
- ✅ 进度显示
- ✅ 支持 curl 和 wget
- ✅ 支持 unzip 和 7z
- ✅ 自动清理临时文件

## 🔄 更新字体

重新运行安装脚本即可更新到最新版本：

```bash
./install-fonts.sh
```

或：

```elisp
M-x install-required-fonts
```

---

**注意：** 安装完成后请重启 Emacs 以应用新字体。某些系统可能需要注销/登录才能识别新字体。

**最后更新：** 2025-01-11
