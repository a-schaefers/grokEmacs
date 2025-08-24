# grokEmacs

![logo](splash.png)

<sub><sup>Logo generated with ChatGPT (OpenAI)</sup></sub>

## Quick Start

`git clone https://github.com/a-schaefers/grokEmacs.git ~/.config/emacs` and if you're new, maybe checkout my [how to grok Emacs](https://www.youtube.com/playlist?list=PLFf4Ibrb-mjTcoaVv6orVtH93K47GPrwl) series.

If you encounter an error in the bootstrap such as: `[2025-08-17 14:11:25] fatal: destination path '/home/$USER/.config/emacs/elpaca/repos/treemacs' already exists and is not an empty directory.` It probably is the result of a partial clone + network timeout, leaving repos/treemacs/ half-baked; the re-clone then balked because the dir existed. In this case the solution is `M-x elpaca-delete RET treemacs` and then restarting Emacs to finish the bootstrap process.

## Announcement

This project does not use GitHub Issues. Instead, please submit your improvements via Pull Request.

grokEmacs is a new rewrite, different from Spartan Emacs, but it achieves many of the same goals in a better way. To find Spartan Emacs, see the [archived branch](https://github.com/a-schaefers/spartan-emacs/tree/spartan-emacs-archive).

## ✨ Interactive Initial Setup Wizard 🧙

The very first time you launch **grokEmacs**, you’ll be greeted by a short wizard. Think of it as Emacs asking you a handful of “first date” questions — nothing heavy, just enough to get comfortable. Your answers are written to `~/.config/emacs/grok-opts.el` (a simple file of `setq`s you can edit later).

You can always restart the wizard with `C-u M-x grok--ensure-opts` if you change your mind.

---

### 🪄 What you’ll be asked

The prompts appear one by one in the minibuffer:

1. **📂 Projects Directory**
   > “Where do you keep your code?”
   Enter the folder path (default: `~/repos`). Projectile & Magit will look here.

2. **😈 Evil Mode or 😇 Holy Mode**
   > “Do you want Vim-style keys?”
   Choose Evil if you live in Vim-land, or stick with Emacs defaults.

3. **🧙 Theme Setup**
   > “Do you want to customize the look, or keep it vanilla?”
   - **Skip** → You get pure “angry fruit salad” Emacs.
   - **Continue** → Pick *fancy* or *minimal* styling.

4. **🎨 Fancy vs Minimal**
   - **Fancy** → Install a theme package (default: `doom-themes`), pick a theme (default: `doom-one`), and choose a modeline style (`doom`, `spaceline`, `moody`, or none).
   - **Minimal** → Stick to built-in themes like `modus-operandi` or `modus-vivendi`. A clean modeline shows just the essentials (file, line, mode).

5. **🪟 Transparency**
   > “Want to see through your editor?”
   Type a number `0–99` (lighter = more see-through). Leave blank or `100` for solid.

6. **🔤 Font & 📏 Size**
   > “What font do you like? And how big should it be?”
   Defaults to *Source Code Pro 11* if you’re not picky.

7. **🔢 Line Numbers**
   > “Do you want line numbers?”
   If yes, you’ll also be asked whether you prefer absolute or relative.

---

### 🎩 Fancy Mode

With Fancy enabled, grokEmacs pulls in your chosen theme package, applies your selected theme, and wires up extras:
- Dashboard on startup
- Treemacs sidebar
- Custom modeline (Doom/Spaceline/Moody)
- Window auto-resizing + splash (toggle with `(setq grok-window-pop-enabled nil)`)

![fancy](grok-fancy.jpg)

---

### 📦 Minimal Mode

Minimal mode skips the extra fancy bloat and gives you just enough polish. You’ll pick one of the built-in themes (recommended: `modus-operandi` or `modus-vivendi`), and get a lightweight modeline.

```elisp
(setq grok-use-modeline "none")
```

![minimal](grok-minimal.jpg)

⚡ Tip: If you want to hack things directly, just open `~/.config/emacs/grok-opts.el` after running the wizard.

## Project goals

- **elpaca** package manager + **use-package**
- **gccemacs** native compilation
- **eglot lsp + company** autocompletion
- **magit+projectile** for VC & project awareness
- **flymake** linting (sideline, eldoc-box)
- **treesitter** everywhere as much as possible
- **vertico** fuzzy completion
- *[dape](https://github.com/svaante/dape) (debug adapter protocol support) is planned, coming soon*

## Keybinds

### Holy Mode (Emacs style)

In **Holy mode** you keep Emacs’ world-class defaults, plus a few helpers and a leader system:

- **Emacs defaults** - all the standard `C-x ...` / `M-x ...` bindings.
- **M-m leader** - powered by `which-key` + `general.el`. Press **M-m** to explore menus.
- **Language extras** - when editing code, **M-m** includes `eglot` bindings.
- **crux** - smarter line movement, duplicate lines, kill whole line, etc.
- **paredit** - structural Lisp editing, keeps parentheses balanced.

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

### Treesit to the future

As treesit support improves, we probably will target [combobulate](https://github.com/mickeynp/combobulate) to get a sort-of of paredit-like
experience beyond scheme/lisp language families.

### Additional language-specific binds

Language modes may add their own bindings as needed.
## Assumptions

### emacs versions

- Expects Emacs 30+ --with-native-compilation --with-tree-sitter ...

### terminal

I do not care about Terminal Emacs. If a hardcore `emacs -nw` user wants to improve that experience, I will gladly merge a PR, provided it meets these conditions:

- All GUI-only assumptions must be guarded with (display-graphic-p) checks, and likewise for terminal-only code.
- Clipboard handling must work out of the box, as graphical Emacs already provides, with compatibility across Xorg, Wayland, and macOS.
- Themes must not look terrible in terminal mode - either by disabling global font-lock, or by using a minimal terminal-only theme that lets the terminal handle colors.

## License
[Public Domain (Unlicense)](https://unlicense.org)
