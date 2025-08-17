;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package company-prescient
  :after (company prescient)
  :ensure t
  :demand t
  :config (company-prescient-mode 1))

(use-package company
  :ensure t
  :demand t
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'html-ts-mode-hook 'company-mode) ; apparently not a prog-mode

  (setq company-idle-delay 0))

(provide 'grok-company)
