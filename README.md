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

## Searching Tools

It’s recommended to install the following command-line tools:

- `ag` (a.k.a. the_silver_searcher, a powerful alternative to `grep`).
- `fd` a super-fast alternative to `find`

!TODO: You should also install the Emacs packages ag, ripgrep or rg if you want
to make sure of Projectile’s commands projectile-ag and projectile-ripgrep.

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


