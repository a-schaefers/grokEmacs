;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package transient
  :ensure (transient :host github :repo "magit/transient" :depth nil :main "lisp/transient.el")
  :demand t)

(use-package magit
  ;; :ensure (:wait t)
  :ensure t
  :demand t
  :after transient
  :init (setq magit-repository-directories `((,grok-projects . 1))))

(provide 'grok-magit)
