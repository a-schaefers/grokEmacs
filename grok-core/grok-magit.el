;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package transient
  :if (not (memq 'transient grok-packages-disabled))
  :ensure (transient :host github :repo "magit/transient" :depth nil :main "lisp/transient.el"))

(use-package magit
  :if (not (memq 'magit grok-packages-disabled))
  :ensure t
  :after transient
  :commands (magit-status)
  :init (setq magit-repository-directories `((,grok-projects . 1))))

(provide 'grok-magit)
