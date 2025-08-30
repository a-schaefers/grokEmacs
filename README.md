# grokEmacs

## Quick Start

```sh
git clone https://github.com/a-schaefers/grokEmacs.git ~/.config/emacs
```

![img](bootstrap.gif)

## Announcement

This project does not use GitHub Issues. Instead, please submit your improvements via Pull Request.

grokEmacs is a new rewrite, different from Spartan Emacs, but it achieves many of the same goals in a better way. To find Spartan Emacs, see the [archived branch](https://github.com/a-schaefers/spartan-emacs/tree/spartan-emacs-archive).

## Features

- Setup wizard for themes, fonts, and evil/holy mode
- Optional **Evil mode** with leader key (Space), or Emacs defaults with **M-m**
- **elpaca** + `use-package` for packages
- **Eglot** for LSP, **vertico/corfu/orderless** for completion
- **Magit/projectile**, **flymake**, **treesitter**
- Native compilation
- *[dape](https://github.com/svaante/dape) (debug adapter protocol support) is planned, coming soon*

## Customization

- Initial Setup Wizard writes answers to `~/.config/emacs/grok-opts.el`.
- Optionally Disable modules/packages in grok-opts.el with `grok-core-disabled` or `grok-packages-disabled`.
- Personal overrides live in `grok.d/grok.el` (gitignored).
- Optionally add new `.el` files in `grok.d/` to extend further.

## Keybinds

- **Holy Mode**: Emacs defaults + `M-m` leader (which-key menu, eglot, paredit, crux).
- **Evil Mode**: Vim modal editing + Space leader (which-key menu, evil-collection, commentary, surround, cleverparens). Eglot still lives on M-m unless customized in grok.el.

## Requirements

- Emacs 30+ with native compilation and tree-sitter.
- GUI recommended; terminal support welcome via PRs.

## License

[Unlicense](https://unlicense.org)
