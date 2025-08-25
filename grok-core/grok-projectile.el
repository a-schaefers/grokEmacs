;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package projectile
  :if (not (memq 'projectile grok-packages-disabled))
  :ensure t
  :defer t
  :init
  (setq projectile-project-search-path `((,grok-projects . 1)))
  :hook (elpaca-after-init . projectile-mode)
  :bind (("C-c p" . projectile-command-map)) ; already on SPC and M-m but people I think expect this to be there
  :config
  (projectile-discover-projects-in-search-path)
  (projectile-save-known-projects))

(provide 'grok-projectile)
