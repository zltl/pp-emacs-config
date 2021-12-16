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

```
apt install clangd
# or
apt install ccls
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


