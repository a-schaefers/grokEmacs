# grokEmacs

[how to grok Emacs](https://www.youtube.com/playlist?list=PLFf4Ibrb-mjTcoaVv6orVtH93K47GPrwl)

fork it - clone it - own it.

`git clone https://github.com/a-schaefers/grokEmacs.git ~/.config/emacs`

![init](init.gif)

## Announcement

I've ditched the "framework caretaker" role. This is now **pure minimal** Emacs - one `init.el` plus a handful of `lisp.d/` snippets.
No user hand-holding, no layer abstractions, just Emacs the way I want it.

In many ways, this config comes full circle.
When I was a beginner, my Emacs looked much like this - simple and direct.
Now, with years of experience, I’ve returned to that approach with a *beginner’s mind*, applying what I’ve learned to create a stable, advanced Emacs experience while keeping the simplicity intact.
It’s my hope that it will also help other beginners exploring Emacs along the way, and that they too might always keep the beginner’s mind.

It’s still geared to get programmers up and running with the best Emacs has to offer - minimal, fast, and pragmatic - and I’m keeping it online for anyone who wants to follow along.

### Project goals

- almost **no abstraction**
- **elpaca** package manager
- **use-package** ensure and demand everything up front (avoids many bugs, reduces config complexity, and delivers a snappy Emacs by default, at only ~0.10s startup cost)
- start in **~0.3 seconds** on an old PC
- **gccemacs** native compilation
- **eglot lsp + company** autocompletion
- **magit+projectile** for VC & project awareness
- **flymake** linting
- **treesitter** everywhere
- **vertico** minibuffer fuzzy completion
- uncluttered modeline
- simple theme
- holy mode default, evil mode optional (uncomment the lines in init.el if you want it)
...
- *dape (debug adapter protocol support) is planned, coming soon*

No bullshit.

No plans for IRC, Email, AI, Window Managing, or other extras - the goal is simple:

> Be the best programmer's text editor in the world - using the most light-weight and best, hand-picked tools - ready to be forked, hacked on, and made one's own.

This is a new project entirely, "grokEmacs" has nothing to do with the old spartan-emacs project. But to find spartan-emacs, see https://github.com/a-schaefers/spartan-emacs/tree/spartan-emacs-archive

## License
[Public Domain (Unlicense)](https://unlicense.org)
