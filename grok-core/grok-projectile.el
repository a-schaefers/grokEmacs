;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path `((,grok-projects . 1)))
  :config
  (projectile-discover-projects-in-search-path)
  (projectile-save-known-projects)
  (projectile-mode 1))

(provide 'grok-projectile)
