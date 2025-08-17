;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package better-shell
  :ensure t
  :demand t
  :config
  (when (string= (getenv "SHELL") "/bin/bash")
    (setq explicit-bash-args '("--noediting" "-i" "-l"))

    (add-hook 'shell-mode-hook #'(lambda ()
                                   (interactive)
                                   (setq comint-input-ring-file-name "~/.bash_history")
                                   (comint-read-input-ring t)
                                   (comint-write-input-ring)))))

(use-package eat
  ;; https://codeberg.org/akib/emacs-eat
  ;;  To setup shell integration for GNU Bash, put the following at the end of your .bashrc:

  ;; [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  ;;   source "$EAT_SHELL_INTEGRATION_DIR/bash"

  ;; For Zsh, put the following in your .zshrc:

  ;; [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  ;;   source "$EAT_SHELL_INTEGRATION_DIR/zsh"
  :ensure t
  :demand t)

(provide 'grok-terminal)
