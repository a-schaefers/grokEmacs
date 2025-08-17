;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package projectile
  :ensure t
  :demand t
  :config
  (projectile-mode 1)

  (with-eval-after-load 'magit
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    (projectile-save-known-projects)))

(provide 'grok-projectile)
