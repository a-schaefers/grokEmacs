# grokEmacs

[how to grok Emacs](https://www.youtube.com/playlist?list=PLFf4Ibrb-mjTcoaVv6orVtH93K47GPrwl)

fork it - clone it - own it.

`git clone https://github.com/a-schaefers/grokEmacs.git ~/.config/emacs`

## Announcement

I've ditched the "framework caretaker" role.

This project does not use GitHub Issues. Instead, please submit your improvements via Pull Request.

grokEmacs is a new rewrite, different from Spartan Emacs, but it achieves many of the same goals in a better way. To find Spartan Emacs, see the [archived branch](https://github.com/a-schaefers/spartan-emacs/tree/spartan-emacs-archive).

## Interactive Initial Setup

On first launch, grokEmacs asks a few questions (projects dir, evil mode, theme, light/dark variant, font, line numbers, etc.) and saves your answers to `~/.config/emacs/grok-opts.el`. You can re-run anytime with `C-u M-x grok--ensure-opts`. Options are just setqs, so you can edit them by hand too.

Fancy uses the doom-one theme variants and doom modeline + treemacs.

![fancy](grok-fancy.jpg)

While minimal uses the built-in modus theme variants with an uncluttered modeline

![minimal](grok-minimal.jpg)

## Project goals

- almost **no abstraction**
- **elpaca** package manager
- **use-package** ensure and demand everything up front (avoids bugs, reduces config complexity, and delivers a snappy Emacs by default)
- start in ~1s on an old PC
- **gccemacs** native compilation
- **eglot lsp + company** autocompletion
- **magit+projectile** for VC & project awareness
- **flymake** linting
- **treesitter** everywhere as much as possible
- **vertico** minibuffer fuzzy completion
- *dape (debug adapter protocol support) is planned, coming soon*

> Be the best programmer's text editor in the world - using the most light-weight and best, hand-picked tools - ready to be forked, hacked on, and made one's own.

## License
[Public Domain (Unlicense)](https://unlicense.org)
