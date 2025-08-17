;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(electric-pair-mode 1)

(add-hook 'before-save-hook 'whitespace-cleanup)

(delete-selection-mode)

(setq-default
 gnutls-verify-error t
 gnutls-min-prime-bits 2048
 password-cache-expiry nil

 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 require-final-newline t
 ediff-window-setup-function 'ediff-setup-windows-plain

 tramp-default-method "ssh"
 tramp-copy-size-limit nil

 vc-follow-symlinks t

 ring-bell-function 'ignore)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq delete-old-versions t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(setq tab-always-indent 'complete)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq scroll-step 1)

(setq pixel-scroll-precision-mode t)

(global-goto-address-mode 1)

(provide 'grok-defaults)
