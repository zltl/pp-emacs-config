A clean and fast emacs config
---

This is my emacs configuration tree. It may be a good starting point for other
emacs users, especially Golang/C/C++/Rust programers.

Emacs itself comes with support for many programming languages. This config adds
improved defaults and extended support for the following, listed in the
approximate order of how much I use them, from most to least:

- Golang
- C/C++
- Rust
- SQL
- Python
- Javascript/Typescript/HTML/CSS/LESS/SASS/SCSS
- HTML/Markdown/Latex

# Requirements

To make the most of the programming language-specific support in this config,
further programs will likely be required.

## Fonts
Recomment [source-code-pr](https://github.com/adobe-fonts/source-code-pro).

If your are using WSL, create `/etc/fonts/local.conf` and set contents below,
then `fc-cache -f -v`:

```xml
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
    <dir>/mnt/c/Windows/Fonts</dir>
</fontconfig>
```

Install [Source Han Sans](https://github.com/adobe-fonts/source-han-sans/tree/release) chinese fonts
manually. Just copy the otf file into `~/.local/share/fonts/` then 
`fc-cache -f -v`. If you work on windows, open the `otf` file then click
`Install`.

Install [all-the-icons](https://github.com/domtronn/all-the-icons.el) fonts 
manually if you want fancy modeline. Just copy `all-the-icons/fonts/*.tty` into 
`~/.local/share/fonts/` then `fc-cache -f -v`. If you work on windows, right 
click `*.tty` then select "Install for all users."

Install [Source Code Pro](https://github.com/adobe-fonts/source-code-pro) like fonts before.

## Golang
Go to [Golang Installation Page](https://go.dev/doc/install) to install the
language. 

You may set a goproxy, especially in Chinese Mainland:

``` bash
export GOPROXY=https://goproxy.io,direct
```

To drive lsp-mode, we need gopls:

```
go install golang.org/x/tools/gopls@latest
```

## C/C++

In order to generate `compile_commands.json` for Emacs itself I have found that
[Bear](https://github.com/rizsotto/Bear) works fine.


```bash
apt-get install bear
```

To generate `compile_commands.json`:

```bash
bear -- <your-build-command>
```

For cmake, You don't need to use Bear for that. Just pass
`-DCMAKE_EXPORT_COMPILE_COMMANDS=ON` flag when you call `cmake`.

Install a language server, `ccls` or `clangd`. `clangd` works faster, and
sometimes works abnormally. I recommend `ccls` if you have a costly computer.

By using `clangd`, you cannot use `lsp-find-implementation`, and
`lsp-find-definition` will go to header files if the implementation file not
open yet. If `M-.` jump to the header file, try switch to the implementation
file `ff-find-related-file` (`M-/`) first, then you can use `M-.` to jump to the
implementation point. It's not always work, in that case, open `.c/.cc/.cpp`
files that related to current work first.

```bash
# use =apt search clangd= to search the latest version
apt install clangd-12 && update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-12 100
# or
apt install ccls
```

Install `cpplint`:

```bash
pip install cpplint
```

Install `clang-format` for `lsp-format-buffer`:

```bash
# user =apt search clang-format= to search the latest versoin
# https://clangd.llvm.org/installation.html
apt install clang-format-12 && update-alternatives --install /usr/bin/clang-format clang-format /usr/bin/clang-format-12 100
```

## Python

Install `pylint` any way:

```bash
pip install pylint
```

## Rust

[Install Rust] (https://www.rust-lang.org/tools/install) first.

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

It’s recommended to install the following command-line tools:

- `ag` (a.k.a. the_silver_searcher, a powerful alternative to `grep`).
- `fd` a super-fast alternative to `find`

```
apt-get install silversearcher-ag
apt install fd-find
```

# Installation

To install, clone this repo to `~/.emacs.d`, i.e. ensure that the `init.el`
contained in this repo ends up at `~/.emacs.d/init.el`:

``` bash
cd ~
git clone https://github.com/zltl/pp-emacs-config
cp -rf .emacs.d backup.emacs.d
ln -sfn pp-emacs-config .emacs.d
```

Upon starting up Emacs for the first time, further third-party packages will be
automatically downloaded and installed. If you encounter any errors at that
stage, try restarting Emacs, and possibly running `M-x package-refresh-contents`
before doing so.

# Updates

To update the third-party packages, `M-x package-list-packages`, then `U`
followed by `x`.


