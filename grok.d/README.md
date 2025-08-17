# grok.d/

This directory is **auto-scanned and `require`d** at startup.

- Drop `.el` files here to extend your config.
- Each file **must end with a matching `(provide 'feature)`**.
- Filenames become features (e.g. `icons.el` → `(provide 'icons)` → `(require 'icons)`).

## Example: `icons.el`

```elisp
;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package all-the-icons
  :ensure t
  :demand t)

(use-package all-the-icons-dired
  :ensure t
  :demand t
  :config
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(provide 'icons)
```
