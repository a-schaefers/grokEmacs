;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package emacs
  :if (not (memq 'shell-mode-grok-config grok-packages-disabled))
  :ensure nil
  :config
  ;; use a login shell and use history file from shell mode, if bash
  (when (string= (getenv "SHELL") "/bin/bash")
    (setq explicit-bash-args '("--noediting" "-i" "-l"))
    (add-hook 'shell-mode-hook #'(lambda ()
                                   (interactive)
                                   (setq comint-input-ring-file-name "~/.bash_history")
                                   (comint-read-input-ring t)
                                   (comint-write-input-ring)))))

(use-package eat
  :if (not (memq 'eat grok-packages-disabled))
  :ensure t)

(provide 'grok-terminal)
