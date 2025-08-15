(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(global-eldoc-mode 1)

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

(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(setq tab-always-indent 'complete)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(or (getenv "EDITOR")
    (progn
      (setenv "EDITOR" "emacsclient")
      (setenv "VISUAL" (getenv "EDITOR"))))

;; Set PAGER to cat, for proper viewing of man pages, etc. while in M-x shell
(or (getenv "PAGER")
    (setenv "PAGER" "cat"))

(add-hook 'elpaca-after-init-hook #'(lambda ()
                               (interactive)
                               (require 'server)
                               (or (server-running-p)
                                   (server-start))))

(setq scroll-step 1)
