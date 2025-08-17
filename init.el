;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(add-to-list 'load-path (expand-file-name "grok-core" user-emacs-directory))

;; C-u M-x grok--ensure-opts will reconfigure
(require 'grok-bootstrap)

(dolist (f '(
             grok-elpaca
             grok-theme
             grok-defaults
             grok-environment
             grok-better-scratch
             grok-binds
             grok-vertico
             grok-holy
             grok-evil
             grok-eglot
             grok-flymake
             grok-magit
             grok-projectile
             grok-company
             grok-terminal
             grok-treesit
             grok-lang
             grok-lang-c-cpp
             ))
  (require f))

(setq grok-d (expand-file-name "grok.d" user-emacs-directory))

(unless (file-directory-p grok-d)
  (make-directory grok-d))

(add-to-list 'load-path grok-d)

(dolist (file (directory-files grok-d nil "\\.el\\'"))
  (require (intern (file-name-base file))))
