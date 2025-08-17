;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package evil
  :if (bound-and-true-p grok-evil)
  :ensure t
  :demand t
  :config (evil-mode 1))

(use-package evil-commentary
  :if (bound-and-true-p grok-evil)
  :ensure t
  :demand t
  :after (evil)
  :config (evil-commentary-mode 1))

(provide 'grok-evil)
