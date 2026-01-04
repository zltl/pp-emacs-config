# A Clean and Fast Emacs Configuration

[![Emacs](https://img.shields.io/badge/Emacs-30+-purple.svg)](https://www.gnu.org/software/emacs/)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

> A modern, modular Emacs configuration optimized for professional development.

**⚠️ IMPORTANT:** This configuration requires **Emacs 30 or higher**.

## 🎯 Overview

This is a well-organized Emacs configuration designed for developers, especially those working with:

**Primary Languages:**
- 🐹 Go
- ⚙️ C/C++
- 🦀 Rust

**Also Supported:**
- 🐍 Python
- 📜 JavaScript/TypeScript
- 🌐 HTML/CSS/LESS/SASS/SCSS
- 📝 Markdown/LaTeX
- 💾 SQL
- And many more...

## ✨ Highlights

> 📢 **Latest Update (2024-12-24):** Added GitHub Copilot CLI deep integration with `eat` terminal

- 🚀 **Fast Startup** - Optimized with lazy loading (30+ packages deferred) and native compilation
- 📁 **Modular Structure** - 22 modules organized in 6 categories (v3.0) ⭐
- 🎨 **Modern UI** - Beautiful themes, icons, and mode line
- ⚙️ **Easy Customization** - 40+ customizable variables via `M-x customize-group RET ltl`
- 🤖 **AI-Powered** - GitHub Copilot and ChatGPT integration
- 🔍 **Smart Completion** - Corfu + Vertico + LSP
- 🌳 **Tree-sitter** - Modern syntax highlighting for 30+ languages
- 📦 **Elpaca 0.11** - Modern async package manager
- 🎯 **Project-Aware** - Projectile integration
- 🏥 **Health Check** - Built-in diagnostic tool (`M-x ltl/doctor`)

---

# 📦 Requirements

## System Requirements

- **Emacs 30+** (required)
- **Node.js** (for Copilot and some language servers)
- **Git** (required)

## 🔤 Fonts

This configuration uses specific fonts for the best visual experience.

### Required Fonts

1. **[Source Code Pro](https://github.com/adobe-fonts/source-code-pro)** - Main programming font
2. **[Source Code Pro Nerd Font](https://www.nerdfonts.com/)** - Programming font with icons
3. **[Droid Sans Fallback](http://fonts3.com/fonts/d/Droid-Sans-Fallback.html)** - CJK characters support
4. **[Source Han Sans CN](https://github.com/adobe-fonts/source-han-sans)** - Chinese font (思源黑体)
5. **[all-the-icons](https://github.com/domtronn/all-the-icons.el)** - Icon fonts (installed automatically)

### Automatic Installation (Recommended) ⭐

**Method 1: Using Elisp script**

In Emacs:
```elisp
M-x load-file RET ~/.emacs.d/install-fonts.el RET
M-x install-required-fonts RET
```

Or from command line:
```bash
emacs --batch -l ~/.emacs.d/install-fonts.el
```

**Method 2: Using Shell script**

```bash
cd ~/.emacs.d
./install-fonts.sh
```

### Check Font Installation

In Emacs:
```elisp
M-x load-file RET ~/.emacs.d/install-fonts.el RET
M-x check-required-fonts RET
```

### Manual Installation

See [FONT_INSTALLATION.md](FONT_INSTALLATION.md) for detailed platform-specific instructions.

---

# 🚀 Installation

## Quick Start

```bash
cd ~
# Backup existing config if you have one
[ -d .emacs.d ] && mv .emacs.d .emacs.d.backup

# Clone this repository
git clone https://github.com/zltl/pp-emacs-config .emacs.d

# Start Emacs (first launch will install packages)
emacs
```

**First Launch Notes:**
- Initial startup takes 2-3 minutes (installing packages)
- If you see errors, restart Emacs
- Tree-sitter grammars install automatically
- Some packages may require Node.js

---

# ⚙️ Language-Specific Setup

## 🤖 GitHub Copilot

### Copilot Inline Completion
**Requirements:** Node.js must be installed.

**Setup:**
1. Start Emacs
2. Run `M-x copilot-login`
3. Follow the browser instructions to authenticate with GitHub

**Usage:**
- `M-`` - Trigger completion
- `TAB` - Accept suggestion
- `M-n`/`M-p` - Next/previous suggestion
- `C-c t `` - Toggle Copilot mode

### Copilot CLI (Terminal Agent) ⭐ NEW
**Requirements:** GitHub Copilot CLI installed (`copilot` command).

**Install Copilot CLI:**
```bash
# Install via npm
npm install -g @anthropic-ai/copilot

# Or via GitHub CLI extension
gh extension install github/gh-copilot
```

**Usage (C-c c prefix):**
| Key | Command | Description |
|-----|---------|-------------|
| `C-c c c` | `copilot-cli` | Start Copilot CLI session |
| `C-c c s` | `copilot-cli-send-buffer` | Send current buffer |
| `C-c c r` | `copilot-cli-send-region` | Send selected region |
| `C-c c e` | `copilot-cli-explain` | Explain code |
| `C-c c v` | `copilot-cli-review` | Review code |
| `C-c c f` | `copilot-cli-fix-error` | Fix errors (with flycheck/flymake) |
| `C-c c t` | `copilot-cli-generate-tests` | Generate unit tests |
| `C-c c R` | `copilot-cli-refactor` | Refactor with instruction |
| `C-c c q` | `copilot-cli-quit` | Quit CLI session |

**In CLI Terminal:**
- `C-c C-b` - Insert @file reference to source buffer
- `C-c C-s` - Insert @file reference (choose file)
- `C-c C-q` - Quit

**Features:**
- Uses `eat` terminal emulator for proper terminal handling
- Side-by-side layout: source code on left, CLI on right
- Auto project directory detection
- Integrates with flycheck/flymake for error context

## 🐹 Go
**Install Go:**
```bash
# Visit https://go.dev/doc/install for official instructions
# Or use your package manager

# Gentoo
sudo emerge --ask dev-lang/go

# Ubuntu/Debian
sudo apt install golang-go

# macOS
brew install go
```

**Install Language Server:**
```bash
go install golang.org/x/tools/gopls@latest
```

**For Chinese users (optional):**
```bash
export GOPROXY=https://proxy.golang.com.cn,direct
```

**Features:**
- Auto-formatting on save
- Auto-import organization
- LSP features (goto definition, find references, etc.)

## ⚙️ C/C++

**Install Language Server (choose one):**

**Option 1: clangd (recommended - faster)**
```bash
# Gentoo
sudo emerge --ask sys-devel/clang

# Ubuntu/Debian (use apt search clangd to find latest version)
sudo apt install clangd-14
sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-14 100

# macOS
brew install llvm
```

**Option 2: ccls (more features, slower)**
```bash
# Ubuntu/Debian
sudo apt install ccls

# macOS
brew install ccls
```

**Install Additional Tools:**
```bash
# clang-format for code formatting
sudo apt install clang-format-14
sudo update-alternatives --install /usr/bin/clang-format clang-format /usr/bin/clang-format-14 100

# cpplint for linting
pip install cpplint
```

**Generate compile_commands.json:**

For CMake projects:
```bash
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
```

For Make projects (using Bear):
```bash
# Install Bear
sudo apt install bear  # Ubuntu/Debian
sudo emerge --ask dev-util/bear  # Gentoo

# Generate compile_commands.json
bear -- make
```

**Tips:**
- `M-.` - Jump to definition
- `M-/` - Switch between header and implementation (`ff-find-related-file`)
- If `M-.` jumps to header, open the `.cpp` file first, then try again

## 🐍 Python

**Install Language Server:**
```bash
pip install 'python-lsp-server[all]' pylsp-mypy pyls-isort pylsp-rope
# Or just use pyright (already configured)
```

**Install Linting Tools:**
```bash
pip install pylint flake8 black
```

**Features:**
- Auto-formatting with black
- Type checking with mypy
- Import sorting
- Virtual environment detection (via `pet` package)

## 🦀 Rust

[Install Rust](https://www.rust-lang.org/tools/install) first.

For Chinese mainlan user, change cargo mirror as below in file 
`~/.cargo/config`:


```toml
# https://mirrors.tuna.tsinghua.edu.cn/help/crates.io-index.git/
[source.crates-io]
registry = "https://github.com/rust-lang/crates.io-index"

replace-with = 'tuna'
[source.tuna]
registry = "https://mirrors.tuna.tsinghua.edu.cn/git/crates.io-index.git"

[net]
git-fetch-with-cli = true
```

I recomand `rust-analyzer` then `rls`, Make sure that ~/.local/bin is listed in
the $PATH variable and use the appropriate URL if you’re not on a x86-64 system.

```bash
mkdir -p ~/.local/bin
curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz | gunzip -c - > ~/.local/bin/rust-analyzer
chmod +x ~/.local/bin/rust-analyzer

# put this into ~/.bashrc
# export PATH=$PATH:~/.local/bin
```

## Searching Tools

**Install these tools for better search experience:**

```bash
# The Silver Searcher (ag) - Fast code search
sudo apt install silversearcher-ag  # Ubuntu/Debian
sudo emerge --ask sys-apps/the_silver_searcher  # Gentoo
brew install the_silver_searcher  # macOS

# fd - Modern find alternative
sudo apt install fd-find  # Ubuntu/Debian
brew install fd  # macOS

# ripgrep (rg) - Another excellent search tool
sudo apt install ripgrep  # Ubuntu/Debian
brew install ripgrep  # macOS
```

## ⌨️ Input Method (Chinese)
This configuration uses [emacs-rime](https://github.com/DogLooksGood/emacs-rime) for Chinese input.

**Install librime:**
```bash
# Gentoo
sudo emerge --ask app-i18n/librime

# Ubuntu/Debian
sudo apt install librime-dev

# macOS
brew install librime
```

**Usage:**
- `C-\` - Toggle input method
- `M-x rime-select-schema` - Switch input schema

---

# 🔧 Updates & Maintenance

## Update Packages

**Recommended Method (using Elpaca commands):**
```elisp
;; In Emacs:
M-x elpaca-fetch-all    ; Fetch updates for all packages
M-x elpaca-merge-all    ; Merge/update all packages
;; Or update specific package:
M-x elpaca-update       ; Update package at point in manager
```

**Alternative Method (complete reinstall):**
```bash
# Delete package directory
rm -rf ~/.emacs.d/elpaca/

# Restart Emacs (packages will reinstall automatically)
emacs
```

**Check for Elpaca updates:**
```elisp
M-x elpaca-manager  ; Then find 'elpaca' package and update it
```

## Clear Cache

```bash
# Clear native compilation cache
rm -rf ~/.emacs.d/eln-cache/

# Clear LSP cache
rm -rf ~/.emacs.d/var/lsp/

# Clear all caches
rm -rf ~/.emacs.d/var/
```

## Troubleshooting

**Problem: Slow startup**
- Run `M-x benchmark-init/show-durations-tabulated` to identify slow packages
- Check `*Messages*` buffer for warnings

**Problem: LSP not working**
- Ensure language server is installed (e.g., `gopls`, `clangd`, `rust-analyzer`)
- Check LSP logs: `M-x lsp-workspace-show-log`
- Restart LSP: `M-x lsp-workspace-restart`

**Problem: Tree-sitter not working**
- Run `M-x mp-setup-install-grammars` to install grammars
- Check if grammars are installed: `M-x treesit-language-available-p`

**Problem: Fonts not displaying correctly**
- Ensure all required fonts are installed
- Restart Emacs after installing fonts
- Run `M-x all-the-icons-install-fonts` if icons are missing

## 📚 Documentation Guides

This configuration comes with comprehensive documentation:

### 🎯 Getting Started
- **[README.md](README.md)** - You are here! Main overview
- **[CHECKLIST.md](CHECKLIST.md)** - Step-by-step setup checklist

### 📖 Reference
- **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** ⭐ - Command cheat sheet
  - Common commands for all features
  - Important variables
  - Emergency fixes
  - Common scenarios

- **[ELPACA_GUIDE.md](ELPACA_GUIDE.md)** - Package manager reference
  - Package installation and management
  - Common commands and workflows
  - Troubleshooting package issues

- **[FONT_INSTALLATION.md](FONT_INSTALLATION.md)** - Font installation guide
  - Automatic font installation scripts
  - Manual installation instructions
  - Platform-specific guides
  - Troubleshooting

### 🧪 Testing & Validation
- **[TESTING.md](TESTING.md)** - Testing procedures
  - Startup tests, health checks
  - Module loading verification
  - Feature testing for LSP, Git, completion
  - Performance benchmarks

## Configuration Health Check

Run the built-in diagnostic tool to check your configuration:

```elisp
M-x ltl/doctor
```

This will check:
- ✓ Emacs version
- ✓ Required programs (git, ag, fd, node, etc.)
- ✓ Fonts installation
- ✓ Language servers (gopls, clangd, rust-analyzer, etc.)
- ✓ Directory structure
- ✓ Elpaca package manager
- ✓ Tree-sitter grammars

Example output:
```
═══════════════════════════════════════════════════════
  Emacs Configuration Health Check
  2025-10-11 20:00:00
═══════════════════════════════════════════════════════

✓ Emacs version: 30.0.50
✓ Directory exists: lisp
✓ Directory exists: etc
✓ Elpaca is loaded
  Installer version: 0.11
✓ All required programs found
✓ Found programs: git, ag, node
✓ All required fonts installed
✓ Found language servers:
  - gopls (Go language server)
  - clangd (C/C++ language server)
✓ Tree-sitter is available
  ✓ Grammar installed: go
  ✓ Grammar installed: python

═══════════════════════════════════════════════════════
  Summary
═══════════════════════════════════════════════════════
  ✓ Emacs Version
  ✓ Directories
  ✓ Elpaca
  ✓ Executables
  ✓ Fonts
  ✓ Language Servers
  ✓ Tree-sitter

Checks passed: 7/7
Time elapsed: 0.15 seconds
═══════════════════════════════════════════════════════

✓ All checks passed! Your configuration is healthy.
```

---

# 📁 Configuration Structure (v3.0)

## Modular Organization

The configuration is organized into **6 logical categories** with **22 modules**:

```
.emacs.d/
├── init.el                 # Main entry point
├── early-init.el           # Early initialization
├── custom.el              # Auto-generated customizations (gitignored)
├── README.md              # This file
├── LICENSE                # MIT License
│
├── lisp/                  # Configuration modules (organized by category)
│   ├── core/              # Core functionality
│   │   ├── init-const.el      # System constants and detection
│   │   ├── init-elpa.el       # Elpaca package manager
│   │   ├── init-custom-vars.el # 40+ customizable variables
│   │   └── init-doctor.el     # Health check tool
│   │
│   ├── ui/                # User interface
│   │   ├── init-themes.el     # Themes, fonts, icons
│   │   └── init-dashboard.el  # Startup dashboard
│   │
│   ├── editing/           # Editing functionality
│   │   ├── init-editor.el     # Core editing features
│   │   ├── init-text.el       # Text manipulation
│   │   └── init-complete.el   # Completion (Corfu/Vertico)
│   │
│   ├── tools/             # Development tools
│   │   ├── init-bindings.el   # Global keybindings
│   │   ├── init-files.el      # File management, Projectile
│   │   ├── init-git.el        # Git integration (Magit)
│   │   └── init-search.el     # Search tools (ag/swiper)
│   │
│   ├── lang/              # Programming language support ⭐
│   │   ├── init-programing-core.el      # LSP, Tree-sitter, tools
│   │   ├── init-programing-systems.el   # C/C++, Rust, Go
│   │   ├── init-programing-web.el       # JS/TS, Tailwind
│   │   ├── init-programing-scripting.el # Python, Lua
│   │   └── init-programing-misc.el      # Other languages
│   │
│   └── apps/              # Applications
│       ├── init-applications.el # Dictionary, UUID, Rime, etc.
│       ├── init-copilot.el      # AI inline completion
│       ├── init-copilot-cli.el  # GitHub Copilot CLI integration ⭐ NEW
│       ├── init-org.el          # Org-mode
│       └── init-shell.el        # Terminal (Eat/Eshell/Vterm)
│
├── etc/                   # Package configuration files
│   ├── eshell/           # Eshell history/aliases
│   └── yasnippet/        # Snippet templates
│
├── var/                   # Variable data (generated)
│   ├── history           # Command history
│   ├── lsp/              # LSP server data
│   ├── org/              # Org-mode caches
│   └── projectile/       # Projectile caches
│
├── elpaca/               # Package manager data
│   ├── builds/           # Built packages
│   └── repos/            # Package source repositories
│
├── eln-cache/            # Native compilation cache (Emacs 28+)
└── tree-sitter/          # Tree-sitter grammars
```

## 🔧 Configuration Modules (v3.0)

### Core (`lisp/core/`)
- **init-const.el** - System constants (OS detection, Emacs version checks)
- **init-elpa.el** - Elpaca package manager setup and `use-package` integration
- **init-custom-vars.el** - 40+ user-customizable variables (`M-x customize-group RET ltl`)
- **init-doctor.el** - Health check diagnostic tool (`M-x ltl/doctor`)

### UI (`lisp/ui/`)
- **init-themes.el** - Theme (Spacemacs Dark), fonts, mode line (doom-modeline)
- **init-dashboard.el** - Startup dashboard with recent files and projects

### Editing (`lisp/editing/`)
- **init-editor.el** - Core editing (smartparens, multiple-cursors, treemacs, ace-window)
- **init-complete.el** - Completion (Corfu, Vertico, Consult, Yasnippet, Flycheck)
- **init-text.el** - Text manipulation (spell checking with jinx)

### Tools (`lisp/tools/`)
- **init-bindings.el** - Keybindings and which-key
- **init-files.el** - File handling (recentf, projectile, dired)
- **init-search.el** - Search tools (swiper, counsel, ag)
- **init-git.el** - Git integration (Magit, git-link, git-timemachine)

### Language Support (`lisp/lang/`) ⭐
- **init-programing-core.el** - Core infrastructure (Tree-sitter, LSP, Combobulate, dev tools)
- **init-programing-systems.el** - Systems languages (C/C++, Rust, Go, Protobuf)
- **init-programing-web.el** - Web development (TypeScript/JavaScript, Tailwind CSS)
- **init-programing-scripting.el** - Scripting (Python, Lua, Shell)
- **init-programing-misc.el** - Other languages (Scala, Haskell, Julia, etc.) and config files

### Applications (`lisp/apps/`)
- **init-applications.el** - Utilities (Dictionary, UUID, Rime input, etc.)
- **init-copilot.el** - GitHub Copilot inline completion
- **init-copilot-cli.el** - GitHub Copilot CLI integration (terminal agent) ⭐ NEW
- **init-org.el** - Org-mode with modern styling and export features
- **init-shell.el** - Terminal emulation (Eat, Eshell, Vterm)

## 🎯 Key Features

### Package Management
- **Elpaca 0.11** - Modern async package manager
  - Asynchronous, parallel package installation
  - Flexible UI for package management (`M-x elpaca-manager`)
  - Direct source downloads for easy development
  - Supports MELPA, GNU ELPA, NonGNU ELPA, and Org repos
  - `use-package` integration with `:ensure` support

### Completion & Navigation
- **Corfu** - Fast in-buffer completion
- **Vertico** - Vertical minibuffer completion
- **Consult** - Powerful search commands
- **Avy** - Jump to visible text

### Programming Support
- **Eglot** - Language Server Protocol (built-in) for Go, C/C++, Python, TypeScript, etc.
- **Tree-sitter** - Modern syntax highlighting
- **Flycheck** - Real-time error checking
- **Projectile** - Project management

### Git Integration
- **Magit** - Full-featured Git interface
- **magit-todos** - TODO tracking

### AI Assistance
- **GitHub Copilot** - AI pair programming (inline completion)
- **GitHub Copilot CLI** - Terminal-based AI agent for code tasks ⭐ NEW
- **ChatGPT Shell** - Interactive AI assistant

## 🔑 Important Keybindings

### Custom Prefix Keys
- `C-c f` - **File operations**
  - `C-c f r` - Recent files
- `C-c t` - **Toggles**
  - `C-c t $` - Spell check (jinx-mode)
  - `C-c t `` - Copilot mode
  - `C-c t C` - Copilot CLI
  - `C-c t e` - Evil mode
  - `C-c t c` - Copilot chat
- `C-c c` - **Copilot CLI commands** ⭐ NEW
  - `C-c c c` - Start CLI session
  - `C-c c s` - Send buffer
  - `C-c c e` - Explain code
  - `C-c c v` - Review code
  - `C-c c f` - Fix errors
- `C-c j` - **Jump/Goto**
  - `C-c j c` - Jump to char (avy)
  - `C-c j j` - Jump to word (avy)
  - `C-c j l` - Jump to line (avy)
- `C-c p` - **Smartparens** operations
- `C-c m` - **Multiple cursors**
- `C-c l` - **Eglot/LSP commands** (in programming modes)
  - `C-c l r` - Rename symbol
  - `C-c l f` - Format buffer
  - `C-c l a` - Code actions
  - `C-c l d` - Show documentation
  - `C-c l s` - Search symbols

### Essential Bindings
- `C-x g` - Magit status
- `C-s` - Swiper search
- `M-x` - Enhanced command palette
- `M-o` - Switch windows (ace-window)
- `M-0` - Toggle Treemacs
- `C-x u` - Visual undo (vundo)
- `M-`` - Copilot complete

## 🛠️ Maintenance & Debugging

### Update Packages
1. Delete `~/.emacs.d/elpaca/`
2. Restart Emacs (packages reinstall automatically)

### Debugging
- Run: `emacs --debug-init`
- Or: `DEBUG=1 emacs`
- Check `*Messages*` buffer
- Run `M-x benchmark-init/show-durations-tabulated` for load times

### First Launch
- Initial startup is slow (installing packages)
- Tree-sitter grammars install automatically
- Restart if you encounter errors

## 📚 Further Reading

### Configuration Guides
- [📦 Elpaca Quick Reference](ELPACA_GUIDE.md) - Package manager commands and usage
- [🎨 Font Installation Guide](FONT_INSTALLATION.md) - Automatic and manual font installation

### External Documentation
- [Emacs Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/)
- [Magit Manual](https://magit.vc/manual/magit/)
- [Eglot Manual](https://www.gnu.org/software/emacs/manual/html_node/eglot/)
- [Elpaca Package Manager](https://github.com/progfolio/elpaca)
- [use-package Documentation](https://github.com/jwiegley/use-package)

---

# 🤝 Contributing

This is a personal configuration, but contributions are welcome!

## Reporting Issues

Please include:
- Emacs version: `M-x emacs-version`
- OS and version
- Steps to reproduce
- Error messages from `*Messages*` buffer

## Suggesting Features

Open an issue with:
- Clear description of the feature
- Use case/motivation
- Example configuration (if applicable)

## Pull Requests

1. Fork the repository
2. Create a feature branch
3. Test your changes thoroughly
4. Submit a PR with clear description

---

# 📄 License

MIT License - Copyright (c) 2021 Liao Tonglang

See [LICENSE](LICENSE) file for details.

---

# 🙏 Acknowledgments

This configuration is built upon the amazing Emacs community's work:

- [Doom Emacs](https://github.com/doomemacs/doomemacs) - Inspiration for modular structure
- [Spacemacs](https://www.spacemacs.org/) - Theme and some configurations
- [Mickey Petersen](https://www.masteringemacs.org/) - Emacs expertise and best practices
- All package maintainers and contributors

---

# ☕ Support

If you find this configuration helpful, consider:
- ⭐ Starring the repository
- 🐛 Reporting bugs or suggesting improvements
- 📢 Sharing with others

---

**Happy Emacs Hacking! 🚀**


