# A Clean and Fast Emacs Configuration

[![Emacs](https://img.shields.io/badge/Emacs-29+-purple.svg)](https://www.gnu.org/software/emacs/)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

> A modern, modular Emacs configuration optimized for professional development.

**IMPORTANT:** This configuration requires **Emacs 29 or higher**. Emacs 30+ is recommended.

## Overview

This is a well-organized Emacs configuration designed for developers, especially those working with:

**Primary Languages:**
- Go
- C/C++
- Rust

**Also Supported:**
- Python
- JavaScript/TypeScript
- HTML/CSS/LESS/SASS/SCSS
- Markdown/LaTeX
- SQL
- And many more...

## Highlights

> **Latest Update (2026-09):** Multi-backend agent CLI (sessions, `aiTitle`, persisted backend), Eglot/gopls hitch, Go PATH, terminal Eldoc

- **Fast Startup** - Lazy loading, native compilation, and `init-perf` command-loop knobs
- **Modular Structure** - 27 modules in 6 categories (`lisp/core|ui|editing|tools|lang|apps`)
- **Modern UI** - Themes, icons, tab-bar, and doom-modeline
- **Easy Customization** - `ltl/` variables via `M-x customize-group RET ltl`
- **AI-Powered** - minuet inline completion, qodercli/cursor-agent/claude/codex side panel, and gptel
- **Smart Completion** - Corfu + Vertico + Eglot (Flymake diagnostics; Flycheck only in Elisp)
- **Tree-sitter** - Built-in highlighting; grammars install on demand (`M-x ltl/setup-install-grammars`)
- **Elpaca 0.12** - Async package manager
- **Project-Aware** - Built-in project.el integration
- **App Ecosystem** - Elfeed RSS, Forge GitHub, auth-source-pass
- **Health Check** - `M-x ltl/doctor`

---

# Requirements

## System Requirements

- **Emacs 29+** (required; 30+ recommended)
- **Node.js** (for some language servers)
- **Git** (required)

## Fonts

This configuration uses specific fonts for the best visual experience.

### Required Fonts

1. **[Source Code Pro](https://github.com/adobe-fonts/source-code-pro)** - Main programming font
2. **[Source Code Pro Nerd Font](https://www.nerdfonts.com/)** - Programming font with icons
3. **[Droid Sans Fallback](http://fonts3.com/fonts/d/Droid-Sans-Fallback.html)** - CJK characters support
4. **[Source Han Sans CN](https://github.com/adobe-fonts/source-han-sans)** - Chinese font (思源黑体)
5. **[all-the-icons](https://github.com/domtronn/all-the-icons.el)** - Icon fonts (installed automatically)

### Automatic Installation (Recommended)

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

# Installation

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
- Tree-sitter grammars are **not** compiled at startup. Run `M-x ltl/setup-install-grammars` when you need them
- Some language servers require Node.js

---

# Language-Specific Setup

## AI agents (minuet + CLI + gptel)

Three layers, all optional:

| Layer | Module | What it is |
|-------|--------|------------|
| Inline completion | `init-minuet.el` | Ghost-text / minibuffer FIM completion |
| Agent side panel | `init-agent-cli.el` | Full TUI CLIs in vterm, multiple sessions |
| Buffer chat | `init-ai.el` | `gptel` (OpenAI / Claude / Ollama, via `~/.authinfo.gpg`) |

### Inline completion (minuet)

On-demand by default (cheaper). Provider is `openai-fim-compatible` so DeepSeek, OpenRouter, or a local OpenAI-compatible server works.

```bash
export MINUET_ENDPOINT="https://api.deepseek.com/beta/completions"
export MINUET_MODEL="deepseek-chat"
export MINUET_API_KEY_ENV="DEEPSEEK_API_KEY"
export DEEPSEEK_API_KEY="..."
```

- `M-`` — trigger completion
- `M-A` / `M-a` / `M-w` — accept whole / line / word
- `M-e` — dismiss
- `C-c t m` — toggle auto-suggestion (`minuet-auto-suggestion-debounce-delay` 0.6s)

### Agent CLI (qodercli / cursor-agent / claude-code / codex)

A right-hand vterm pane that can run several agent TUIs at once. Do **not** send these CLIs through Eat — Ink-style redraws freeze Emacs. First use compiles `vterm-module` (`M-x ltl/vterm-setup` or `C-c c V`). On Ubuntu/Debian and Arch, missing cmake/libtool packages are installed automatically (sudo may prompt).

**Backends** (first installed is the default until you pick one):

| Backend | Executable | Notes |
|---------|------------|--------|
| `qodercli` | `qodercli` | `--session-id`; task title from `~/.qoder/projects/.../*.jsonl` |
| `cursor-agent` | `agent`, fallback `cursor-agent` | Cursor Agent CLI |
| `claude-code` | `claude` | `--session-id`; `C-c c !` adds `--dangerously-skip-permissions` |
| `codex` | `codex` | OpenAI Codex CLI (`@file` refs). Listed in `C-c c i` even if missing |

`C-c c a` saves the default backend to `var/agent-backend` and restores it on the next Emacs start. After that, `C-c c c` opens **that** backend, not whichever session last had focus.

**Sessions:**

- `C-c c n` starts another session; the old ones stay alive
- `C-c c l` lists live sessions by task summary
- Buffer name follows the summary: `*ai-agent: …*`
- qodercli / claude-code write `aiTitle` into their JSONL transcript; Emacs polls it and renames the buffer
- `C-c c m` sets the summary by hand and locks it so the CLI title cannot overwrite it
- Prefix on `C-c c n`: `C-u` replaces the current session; `C-u C-u` asks for a workdir first
- New sessions reuse the existing right pane (no extra split). TTY width is capped at 72 columns (`ltl/agent-window-width` is 100 on GUI)
- Workdir is project.el / `.git` / `default-directory`, or `C-c c w` / `ltl/agent-directory`

**Keys (`C-c c` prefix, also `C-c t a`):**

| Key | Command | Description |
|-----|---------|-------------|
| `C-c c c` | `ltl/agent` | Focus or start the **saved** default backend |
| `C-c c n` | `ltl/agent-new` | Another session (same backend) |
| `C-c c l` | `ltl/agent-select` | Pick a live session by task summary |
| `C-c c m` | `ltl/agent-rename` | Set / lock the task summary |
| `C-c c a` | `ltl/agent-switch` | Switch default backend (persisted) |
| `C-c c i` | `ltl/agent-status` | Show which CLIs are on `PATH` |
| `C-c c V` | `ltl/vterm-setup` | Install/compile vterm module |
| `C-c c w` | `ltl/agent-set-directory` | Workdir for the next new session |
| `C-c c !` | `ltl/agent-claude-dangerous` | New Claude session, skip permissions |
| `C-c c s` | `ltl/agent-send-buffer` | `@file` the current buffer |
| `C-c c r` | `ltl/agent-send-region` | Send the region via a temp file |
| `C-c c e` | `ltl/agent-explain` | Explain region or buffer |
| `C-c c v` | `ltl/agent-review` | Review current buffer |
| `C-c c f` | `ltl/agent-fix-error` | Buffer + Flymake/Flycheck diagnostics |
| `C-c c t` | `ltl/agent-generate-tests` | Generate unit tests |
| `C-c c R` | `ltl/agent-refactor` | Refactor with a typed instruction |
| `C-c c d` | `ltl/agent-diff-current-file` | Magit/VC diff of the current file |
| `C-c c z` | `ltl/agent-fullscreen` | Toggle agent-only layout |
| `C-c c k` | `ltl/agent-stop` | Kill the subprocess, keep the buffer |
| `C-c c x` | `ltl/agent-restart` | Restart same backend / root / flags |
| `C-c c q` | `ltl/agent-quit` | Quit this session; show the next if any |
| `C-c c Q` | `ltl/agent-quit-all` | Quit every session |

**Inside the agent vterm:**

- `C-c C-b` — `@` the source buffer file
- `C-c C-s` — `@` a file you pick
- `C-c C-q` — quit this session
- `C-c C-j` — vterm copy-mode (scroll / select)

**Layout:** source on the left, agent on the right. `C-c t w a` snaps the visible agent pane to its preset width.

Install Codex later with the official CLI (`codex` on `PATH`); until then `C-c c i` shows `[--] codex` and `C-c c a` will not select it.

If an old Eat-backed session is still frozen, kill the `agent` / `qodercli` / `claude` / `codex` child from another shell, then restart Emacs so new sessions use vterm.

### gptel

`C-c a a` opens a chat buffer (`org-mode` by default). `C-c a s` sends, `C-c a m` is the gptel menu. Configure API keys in `~/.authinfo.gpg`.

## Go

Put the toolchain and `go install` binaries on `PATH`. This config prepends `~/.local/go/bin` and `~/go/bin` when you open a Go buffer, because GUI Emacs often inherits `~/.profile` and misses directories that only live in `~/.bashrc`.

**Install Go** (official installer, or keep a user-local tree at `~/.local/go`):
```bash
# Visit https://go.dev/doc/install
# Ubuntu/Debian package Go is often too old for current gopls
# macOS
brew install go
```

**Install tools** (gopls, goimports, delve):
```bash
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest
go install github.com/go-delve/delve/cmd/dlv@latest
```

Or from Emacs: `M-x ltl/go-install-tools`. Opening a `.go` file no longer asks to install tools (that prompt froze `find-file`). A missing tool is mentioned once in the echo area.

**Proxy (optional):**
```bash
export GOPROXY=https://proxy.golang.com.cn,direct
```

`exec-path-from-shell` also copies `GOPATH`, `GOBIN`, and `GOPROXY` on graphical and daemon frames (`mac`, `ns`, `x`, and `pgtk`).

**Eglot / gopls:**
- Starts automatically in `go-mode` / `go-ts-mode`
- Event log off (`eglot-events-buffer-config` `:size 0`; the old `eglot-events-buffer-size` is ignored on Emacs 32)
- Semantic tokens and inlay hints stay opt-in (`M-x eglot-semantic-tokens-mode` if you want them)
- `staticcheck` and analysis progress reporting are off; `vendor`, `.git`, `node_modules`, and `testdata` are filtered
- Hover docs use `eldoc-box` on a GUI. In a terminal, open `*eldoc*` with `C-c l d` (or `C-c l h`). Scroll with `C-c l n` / `C-c l p`. While `*eldoc*` is visible, the hover popup is hidden

**Features:**
- LSP navigation (definition, references, rename, format, code actions)
- Flymake diagnostics (not Flycheck)
- `M-x ltl/profile-cpu` (`C-c t P`) if a first-open hitch comes back

## C/C++

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

## Python

**Install Language Server:**
```bash
# Default mapping is pyright (see ltl/lsp-language-servers)
pip install pyright
# Or: pip install 'python-lsp-server[all]'
```

**Features:**
- Eglot in `python-mode` / `python-ts-mode`
- Virtualenv / Poetry / uv detection via `pet`
- Docstring helpers via `python-docstring`

## Rust

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

## Input Method (Chinese)
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

# Updates & Maintenance

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
- Ensure the language server is on Emacs `exec-path` (e.g. `gopls` in `~/go/bin`)
- Restart Eglot: `M-x eglot-reconnect`
- Event logging is off by default. Temporarily set `eglot-events-buffer-config` to `(:size 2000000 :format full)` and `M-x eglot-reconnect`, then `M-x eglot-events-buffer`
- Go: `M-x ltl/go-install-tools` if `gopls` / `goimports` / `dlv` are missing

**Problem: Emacs freezes for a moment after opening a Go file**
- That is gopls's first workspace dump, not per-keystroke traffic
- Confirm `eglot-events-buffer-config` is `(:size 0 :format short)` and semantic tokens are off
- Profile the hitch: `M-x ltl/profile-cpu` or `C-c t P`

**Problem: Tree-sitter not working**
- Run `M-x ltl/setup-install-grammars` (not done at startup)
- Check a language: `M-: (treesit-language-available-p 'go)`

**Problem: Fonts not displaying correctly**
- Ensure all required fonts are installed
- Restart Emacs after installing fonts
- Run `M-x all-the-icons-install-fonts` if icons are missing

## Documentation Guides

This configuration comes with comprehensive documentation:

### Getting Started
- **[README.md](README.md)** - You are here! Main overview
- **[CHECKLIST.md](CHECKLIST.md)** - Step-by-step setup checklist

### Reference
- **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - Command cheat sheet
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

### Testing & Validation
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
- ✓ Tree-sitter availability (grammars are optional until you install them)

Example output:
```
═══════════════════════════════════════════════════════
  Emacs Configuration Health Check
  2026-09-04 15:00:00
═══════════════════════════════════════════════════════

✓ Emacs version: 32.0.50
✓ Directory exists: lisp
✓ Directory exists: etc
✓ Elpaca is loaded
  Installer version: 0.12
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

# Configuration Structure

## Modular Organization

The configuration is organized into **6 categories** and **27 modules**:

```
.emacs.d/
├── init.el                 # Main entry point
├── early-init.el           # Early initialization
├── custom.el               # Auto-generated customizations (gitignored)
├── README.md               # This file
├── LICENSE                 # MIT License
│
├── lisp/                   # Configuration modules
│   ├── core/
│   │   ├── init-const.el       # OS / Emacs version predicates
│   │   ├── init-elpa.el        # Elpaca 0.12
│   │   ├── init-custom-vars.el # ltl/ customizable variables
│   │   ├── init-doctor.el      # M-x ltl/doctor
│   │   └── init-perf.el        # Redisplay, so-long, profiler
│   │
│   ├── ui/
│   │   ├── init-themes.el      # Theme, fonts, icons, modeline
│   │   └── init-dashboard.el   # Startup dashboard
│   │
│   ├── editing/
│   │   ├── init-editor.el      # Smartparens, multiple-cursors, Treemacs
│   │   ├── init-text.el        # jinx spell-check
│   │   └── init-complete.el    # Corfu, Vertico, Consult, Yasnippet
│   │
│   ├── tools/
│   │   ├── init-bindings.el    # C-c prefix maps, which-key
│   │   ├── init-window-resize.el
│   │   ├── init-files.el       # recentf, project.el, dired
│   │   ├── init-git.el         # Magit, Forge, diff-hl
│   │   └── init-search.el      # Consult / ag
│   │
│   ├── lang/
│   │   ├── init-programming-core.el      # Tree-sitter, Eglot, Eldoc
│   │   ├── init-programming-systems.el   # C/C++, Rust, Go, Protobuf
│   │   ├── init-programming-web.el       # JS/TS, Tailwind
│   │   ├── init-programming-scripting.el # Python, Lua
│   │   └── init-programming-misc.el      # Other languages
│   │
│   └── apps/
│       ├── init-applications.el # Dictionary, UUID, Rime
│       ├── init-minuet.el       # Inline AI completion
│       ├── init-agent-cli.el    # Agent side panel (vterm)
│       ├── init-ai.el           # gptel
│       ├── init-elfeed.el       # RSS
│       ├── init-org.el          # Org-mode
│       └── init-shell.el        # Vterm / Eat / Eshell
│
├── etc/                    # Package configuration files
├── var/                    # Generated caches (no-littering)
├── elpaca/
│   ├── builds/             # Built packages
│   └── sources/            # Package checkouts
├── eln-cache/              # Native compilation cache
└── tree-sitter/            # Installed grammars
```

## Configuration Modules

### Core (`lisp/core/`)
- **init-const.el** - System constants (OS detection, Emacs version checks)
- **init-elpa.el** - Elpaca package manager and `use-package` integration
- **init-custom-vars.el** - User-customizable `ltl/` variables (`M-x customize-group RET ltl`)
- **init-doctor.el** - Health check (`M-x ltl/doctor`)
- **init-perf.el** - Redisplay, font-lock stealth, so-long, `ltl/profile-cpu`

### UI (`lisp/ui/`)
- **init-themes.el** - Theme (Spacemacs Dark), fonts, doom-modeline
- **init-dashboard.el** - Startup dashboard with recent files and projects

### Editing (`lisp/editing/`)
- **init-editor.el** - Smartparens, multiple-cursors, Treemacs, ace-window
- **init-complete.el** - Corfu, Vertico, Consult, Yasnippet; Flycheck only in `emacs-lisp-mode`
- **init-text.el** - Spell checking with jinx

### Tools (`lisp/tools/`)
- **init-bindings.el** - Keybindings and which-key
- **init-window-resize.el** - Persistent resize menu (`C-c t w`)
- **init-files.el** - recentf, project.el, dired
- **init-search.el** - Consult and ag
- **init-git.el** - Magit, magit-todos, Forge, git-link, git-timemachine, diff-hl

### Language Support (`lisp/lang/`)
- **init-programming-core.el** - Tree-sitter, Eglot, Combobulate, Eldoc hover
- **init-programming-systems.el** - C/C++, Rust, Go, Protobuf
- **init-programming-web.el** - TypeScript/JavaScript, Tailwind CSS
- **init-programming-scripting.el** - Python, Lua, Shell
- **init-programming-misc.el** - Other languages and config files

### Applications (`lisp/apps/`)
- **init-applications.el** - Dictionary, UUID, Rime input
- **init-minuet.el** - Inline AI completion via minuet
- **init-agent-cli.el** - qodercli / cursor-agent / claude-code / codex side panel in vterm
- **init-ai.el** - gptel
- **init-elfeed.el** - RSS reader
- **init-org.el** - Org-mode
- **init-shell.el** - Vterm, Eat, Eshell

## Key Features

### Package Management
- **Elpaca 0.12** - Modern async package manager
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
- **Eglot** - Built-in LSP for Go, C/C++, Python, TypeScript, Rust, etc.
- **Flymake** - Diagnostics in Eglot buffers (Flycheck is Elisp-only)
- **Tree-sitter** - Built-in highlighting; grammars installed on demand
- **project.el** - Built-in project management

### Git Integration
- **Magit** - Full-featured Git interface
- **magit-todos** - TODO tracking
- **diff-hl** - Fringe uncommitted changes
- **Forge** - GitHub/GitLab from Magit

### AI Assistance
- **minuet** - Inline ghost-text completion (OpenAI-FIM compatible)
- **Agent CLI** - qodercli / cursor-agent / claude-code / codex side panel
- **gptel** - LLM chat in Emacs

## Important Keybindings

### Custom Prefix Keys
- `C-c f` - **File operations**
  - `C-c f r` - Recent files
- `C-c t` - **Tools and toggles**
  - `C-c t $` - Spell check (jinx-mode)
  - `C-c t m` - minuet auto-suggestion
  - `C-c t a` - Agent CLI
  - `C-c t e` - Evil mode
  - `C-c t r` - Reload configuration
  - `C-c t P` - CPU profile a hitch (`ltl/profile-cpu`)
  - `C-c t t` - Select Treemacs
  - `C-c t v` - Open vterm
  - `C-c t w` - Resize menu
    - `h` / `l` - Narrower / wider
    - `j` / `k` - Shorter / taller
    - `=` - Balance windows
    - `a` - Resize visible Agent pane to its preset
    - `t` - Resize Treemacs to its preset
    - The menu stays visible; exit with `q` or `C-g`
- `C-c c` - **Agent CLI** (qodercli / cursor-agent / claude-code / codex in vterm)
  - `C-c c c` - Focus or start the saved default backend
  - `C-c c n` - New session (`C-u` replace, `C-u C-u` pick workdir)
  - `C-c c l` / `C-c c m` - List sessions by `aiTitle` / rename
  - `C-c c a` - Switch default backend (remembered across restarts)
  - `C-c c !` - Claude skip-permissions
  - `C-c c s` / `r` / `e` / `v` / `f` — send buffer, region, explain, review, fix
  - `C-c c z` / `q` / `Q` — fullscreen / quit one / quit all
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
  - `C-c l d` - Open `*eldoc*` (full symbol documentation)
  - `C-c l h` - Help at point (GUI hover / TTY `*eldoc*`)
  - `C-c l n` / `C-c l p` - Scroll documentation without leaving the source
  - `C-c l s` - Search symbols
- `C-c a` - **gptel**
- `C-c n` - **Denote notes**
- `C-c g` - **Git commands**
- `C-c h` - **Helpful at point**
- `C-c w` - **Perspective workspaces** (when Perspective is enabled)

### Essential Bindings
- `C-x g` - Magit status
- `C-s` - Consult line search
- `M-x` - Enhanced command palette
- `M-o` - Switch windows (ace-window)
- `C-c t t` - Select Treemacs window
- `C-x u` - Visual undo (vundo)
- `M-`` - minuet complete

## Maintenance & Debugging

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
- Tree-sitter grammars are not compiled automatically; run `M-x ltl/setup-install-grammars`
- Restart if you encounter errors

## Further Reading

### Configuration Guides
- [Elpaca Quick Reference](ELPACA_GUIDE.md) - Package manager commands and usage
- [Font Installation Guide](FONT_INSTALLATION.md) - Automatic and manual font installation

### External Documentation
- [Emacs Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/)
- [Magit Manual](https://magit.vc/manual/magit/)
- [Eglot Manual](https://www.gnu.org/software/emacs/manual/html_node/eglot/)
- [Elpaca Package Manager](https://github.com/progfolio/elpaca)
- [use-package Documentation](https://github.com/jwiegley/use-package)

---

# Contributing

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

# License

MIT License - Copyright (c) 2021 Liao Tonglang

See [LICENSE](LICENSE) file for details.

---

# Acknowledgments

This configuration is built upon the amazing Emacs community's work:

- [Doom Emacs](https://github.com/doomemacs/doomemacs) - Inspiration for modular structure
- [Spacemacs](https://www.spacemacs.org/) - Theme and some configurations
- [Mickey Petersen](https://www.masteringemacs.org/) - Emacs expertise and best practices
- All package maintainers and contributors

---

# Support

If you find this configuration helpful, consider:
- Starring the repository
- Reporting bugs or suggesting improvements
- Sharing with others

---

**Happy Emacs Hacking!**


