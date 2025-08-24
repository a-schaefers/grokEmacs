# grokEmacs

![logo](splash.png)

<sub><sup>Logo generated with ChatGPT (OpenAI)</sup></sub>

`git clone https://github.com/a-schaefers/grokEmacs.git ~/.config/emacs` and if you're new, maybe checkout my [how to grok Emacs](https://www.youtube.com/playlist?list=PLFf4Ibrb-mjTcoaVv6orVtH93K47GPrwl) series.

If you encounter an error in the bootstrap such as: `[2025-08-17 14:11:25] fatal: destination path '/home/$USER/.config/emacs/elpaca/repos/treemacs' already exists and is not an empty directory.` It probably is the result of a partial clone + network timeout, leaving repos/treemacs/ half-baked; the re-clone then balked because the dir existed. In this case the solution is `M-x elpaca-delete RET treemacs` and then restarting Emacs to finish the bootstrap process.

---

## Announcement

This project does not use GitHub Issues. Instead, please submit your improvements via Pull Request.

grokEmacs is a new rewrite, different from Spartan Emacs, but it achieves many of the same goals in a better way. To find Spartan Emacs, see the [archived branch](https://github.com/a-schaefers/spartan-emacs/tree/spartan-emacs-archive).

---

## Interactive Initial Setup

On first launch, grokEmacs asks a few setup questions (projects dir, Evil mode, theme style, font, font size, line numbers, transparency, etc.) and saves your answers to `~/.config/emacs/grok-opts.el`. You can rerun the wizard anytime with C-u M-x grok--ensure-opts, or edit the file directly since it’s just a series of setqs. You can also skip the pre-baked theming entirely and stick with pure angry-fruit. In Fancy mode, grokEmacs also resizes the window and pops Treemacs on startup; if you don’t like that, disable it with `(setq grok-window-pop-enabled nil)` in grok-opts 🍻.

---

**Fancy** loads a user-chosen theme package and theme name, defaulting to doom-themes with doom-one if unsure. It also enables Doom, Moody or Spacemacs modeline, Treemacs, Dashboard, and related extras.

![fancy](grok-fancy.jpg)

---

**Minimal** goes through the built-in options of the theme wizard, skipping any extra theme packages and all the fancy bloat. It lets the user pick a built-in theme—recommending modus-vivendi or modus-operandi—and provides a clean, uncluttered modeline that shows only the modified state, filename, line number, and major mode. If you’d rather keep the default Emacs modeline, just set `(setq grok-use-modeline "none")` in grok-opts.

![minimal](grok-minimal.jpg)

---

## Project goals

- **elpaca** package manager + **use-package**
- **gccemacs** native compilation
- **eglot lsp + company** autocompletion
- **magit+projectile** for VC & project awareness
- **flymake** linting (sideline, eldoc-box)
- **treesitter** everywhere as much as possible
- **vertico** fuzzy completion
- *[dape](https://github.com/svaante/dape) (debug adapter protocol support) is planned, coming soon*

---

## Keybinds

### Holy Mode (Emacs style)

In **Holy mode** you keep Emacs’ world-class defaults, plus a few helpers and a leader system:

- **Emacs defaults** - all the standard `C-x ...` / `M-x ...` bindings.
- **M-m leader** - powered by `which-key` + `general.el`. Press **M-m** to explore menus.
- **Language extras** - when editing code, **M-m** includes `eglot` bindings.
- **crux** - smarter line movement, duplicate lines, kill whole line, etc.
- **paredit** - structural Lisp editing, keeps parentheses balanced.

---

### Evil Mode (Vim style)

If you enabled Evil during setup, you get Vim’s modal editing plus a leader system:

- **Evil defaults** - normal/insert/visual modes and Vim-style motions.
- **SPACE leader** - powered by `which-key` + `general.el`. Press **Space** to explore menus.
- **Language extras** - when editing code, **M-m** is available for `eglot` bindings.
- **evil-collection** - makes Magit, Dired, and other modes feel natural in Vim.
- **evil-commentary** - toggle comments with `gc`.
- **evil-surround** - add/change/delete surrounding quotes, parens, tags.
- **evil-matchit** - jump between matching pairs (if/else, HTML tags, etc.).
- **evil-cleverparens** - Lisp structural editing, Vim-style (like paredit).

---

### Treesit to the future

As treesit support improves, we probably will target [combobulate](https://github.com/mickeynp/combobulate) to get a sort-of of paredit-like
experience beyond scheme/lisp language families.

---

### Additional language-specific binds

Language modes may add their own bindings as needed.
## Assumptions

---

### emacs versions

- Expects Emacs 30+ --with-native-compilation --with-tree-sitter ...

---

### terminal

I do not care about Terminal Emacs. If a hardcore `emacs -nw` user wants to improve that experience, I will gladly merge a PR, provided it meets these conditions:

- All GUI-only assumptions must be guarded with (display-graphic-p) checks, and likewise for terminal-only code.
- Clipboard handling must work out of the box, as graphical Emacs already provides, with compatibility across Xorg, Wayland, and macOS.
- Themes must not look terrible in terminal mode - either by disabling global font-lock, or by using a minimal terminal-only theme that lets the terminal handle colors.

---

## License
[Public Domain (Unlicense)](https://unlicense.org)
