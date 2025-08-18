;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package emacs
  :ensure nil
  :init
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq-default gnutls-verify-error t
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
                ring-bell-function 'ignore
                tab-always-indent 'complete
                scroll-step 1
                pixel-scroll-precision-mode t)
  (setq backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
        delete-old-versions t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  :hook ((before-save . whitespace-cleanup)
         (after-save  . executable-make-buffer-file-executable-if-script-p))
  :config
  (when (file-exists-p custom-file) (load-file custom-file))
  (electric-pair-mode 1)
  (delete-selection-mode 1)
  (global-goto-address-mode 1))

(provide 'grok-better-defaults)
