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
- Optional **Evil mode** with leader key (Space) via **which-key** + **General.el**, or Emacs defaults with **M-m**
- **elpaca** + **use-package** for packages
- **Eglot** for LSP, **vertico/corfu/orderless** for completion
- **Flymake** with **sideline/sideline-flymake** for linting
- **Magit/projectile**
- **Treesitter** first approach to modes: if a treesitter mode exists (and for most common languages it does now), it’s used via **treesit-auto**
- Native compilation "just works"
- *[dape](https://github.com/svaante/dape) (debug adapter protocol support) is planned, coming soon*

## Customization

> When asked "🧙 Continue to theme customization wizard?", choosing Skip leaves you with bare vanilla Emacs appearance and no polish. If you continue, the next prompt "🎨 Theme style (✨ fancy | 📦 minimal)" lets you pick: Minimal, which applies a built-in theme and simple modeline for a clean setup, or Fancy, which installs an external theme (of your choice), custom modelines, dashboard, and Treemacs. Both "minimal" and "fancy" offer followup questions to obtain your preferences regarding fonts, line numbering, transparency, etc.

- Initial Setup Wizard writes answers to `~/.config/emacs/grok-opts.el`. For more info, see `C-h v` on the setup variables in `grok-opts.el`.
- Optionally disable modules/packages in grok-opts.el with `grok-core-disabled` or `grok-packages-disabled`.
- Primary and final configuration live in `grok.d/grok.el` (gitignored).
- Optionally add new `.el` files in `grok.d/` to extend further (tracked).

## Keybinds

- **Holy Mode**: Emacs defaults + `M-m` leader (which-key menu, eglot, paredit, crux).
- **Evil Mode**: Vim modal editing + Space leader (which-key menu, evil-collection, commentary, surround, cleverparens). Eglot still lives on M-m unless customized in `grok.el`.

## Requirements

- Emacs 30+ with native compilation and tree-sitter.
- GUI recommended; terminal support welcome via PRs.

## Quick comparison (Doom vs Spacemacs vs Grok)

GrokEmacs is a small, plain-Elisp starter you can own. Doom is a fast, highly-engineered framework with a big module graph. Spacemacs is a full "distribution" with layers and lots of defaults. If you want simple files you can read/change in an afternoon, pick Grok. If you want a giant switchboard of features curated for you, pick Doom or Spacemacs.

### Bugs

GrokEmacs carries fewer packages than Doom or Spacemacs, so there’s less surface area for breakage.
On the other hand, the smaller userbase means fewer eyes to catch edge cases.

The intent is to keep Grok as close to upstream as possible - most issues should really be fixed in the package’s own repo.
Grok itself is just a prepackaged, batteries-included Emacs with sane defaults.

### What sets Grok apart

- scope: tiny codebase; everything is ordinary use-package in a handful of files; no custom macro jungle.

- philosophy: "simplicity > elegance." Defaults first, you customize in grok.d/ and a single grok.el.

- bootstrap: Elpaca + a one-time wizard that writes ~/.emacs.d/grok-opts.el (fonts/theme/evil/line-nums, etc.).

- toggles: two flat switches, grok-core-disabled and grok-packages-disabled, instead of module/layer systems.

- LSP/completion: hard choices, batteries included: Eglot + Flymake + Corfu + Orderless + Vertico + Sideline.

- treesit-first: uses treesit-auto globally; falls back only as needed.

- keys: either Evil with SPC leader or "Holy" with M-m leader (which-key pops only on M-m, so it stays quiet).

### Compared to Doom

- structure: Doom wraps a lot in custom macros/modules and autoloads; Grok stays "just use-package and require."

- size/complexity: Doom’s module system is vast and opinionated; Grok keeps a narrow, readable surface.

- defaults: Doom offers many curated toggles and integrations out of the box; Grok ships a sensible, slimmer IDE stack.

- upgrade surface: Doom gains power but more moving parts; Grok’s fewer abstractions mean fewer "framework" surprises.

- who it fits: Doom for maximal features and a curated ecosystem; Grok if you want a starter you’ll comfortably hack apart.

### Compared to Spacemacs

- model: Spacemacs layers give a "distro" feel with lots pre-wired; Grok avoids layers entirely.

- ergonomics: Spacemacs does Evil/Hybrid/Emacs styles via layers; Grok does a simple Evil vs Holy switch at setup.

- learning curve: Spacemacs is feature-rich but its layer conventions are heavier; Grok keeps Elisp close to the metal.

- customization path: Spacemacs expects layer edits and dotspacemacs; Grok expects you to drop files into grok.d/.

### When to pick which

pick grok if you want:

- a starter that’s small, readable, and easy to diff/own
- Eglot/Flymake + Vertico/Orderless/Corfu done the "boring, modern" way
- quick switches (Evil vs Holy; minimal vs fancy theme) without a module matrix

pick doom if you want:

- Evil-first experience with a high-performance, highly curated framework
- deep module coverage and lots of prebuilt integrations
- an active issue tracker and community

pick spacemacs if you want:

- Evil by default, but also hybrid/vanilla styles via layers
- a comprehensive "big distro" with layers and extensive defaults
- a very large userbase and extensive documentation

### Practical migration mental model

- Doom "enable a module" -> Grok: Core modules are already on with sane defaults. If you don’t want one, list it in grok-packages-disabled. To add something new, drop a small use-package in grok.d/.

- Spacemacs "add a layer" -> Grok: Just drop a file into grok.d/ that ends with (provide 'feature).

- Both "tweak framework knobs" -> Grok: Edit plain Elisp in grok.el.

- Community/support: Doom and Spacemacs have large communities and issue trackers. Grok is intentionally lean - the repo takes pull requests for improvements, but there’s no dedicated support forum. Since it’s just plain use-package Emacs, help is best found in upstream package trackers or in the wider Emacs community (e.g. #emacs on Libera IRC).

## License

[Unlicense](https://unlicense.org)
