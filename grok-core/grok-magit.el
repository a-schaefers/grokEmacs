;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package transient
  :ensure (transient :host github :repo "magit/transient" :depth nil :main "lisp/transient.el"))

(use-package magit
  :ensure t
  :commands (magit-status)
  :after transient
  :init (setq magit-repository-directories `((,grok-projects . 1))))

(provide 'grok-magit)
