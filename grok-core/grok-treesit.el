;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; TODO: https://github.com/mickeynp/combobulate check this out

(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'grok-treesit)
