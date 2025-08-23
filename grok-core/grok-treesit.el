;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; TODO: https://github.com/mickeynp/combobulate check this out

(use-package treesit-auto
  :if (not (memq 'treesit-auto grok-packages-disabled))
  :ensure t
  :hook (elpaca-after-init . global-treesit-auto-mode)
  :custom (treesit-auto-install 'prompt)
  :config (treesit-auto-add-to-auto-mode-alist 'all))

(provide 'grok-treesit)
