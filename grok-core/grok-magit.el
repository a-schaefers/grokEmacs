;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package transient :ensure t :demand t)

(use-package magit
  :ensure (:wait t)
  :demand t
  :after (transient)
  :init (setq magit-repository-directories `((,grok-projects . 1))))

(provide 'grok-magit)
