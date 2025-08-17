;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(add-to-list 'load-path (expand-file-name "grok-core" user-emacs-directory))

;; C-u M-x grok--ensure-opts will reconfigure
(require 'grok-bootstrap)

(dolist (f `(
             grok-elpaca
             grok-theme-globals
             ,@(when (string= grok-theme-fancy "minimal") '(grok-theme-minimal))
             ,@(when (string= grok-theme-fancy "fancy") '(grok-theme-fancy))
             grok-defaults
             grok-environment
             grok-better-scratch
             grok-binds
             grok-vertico
             ,@(unless grok-evil-mode '(grok-holy))
             ,@(when grok-evil-mode '(grok-evil))
             grok-lsp
             grok-magit
             grok-projectile
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
