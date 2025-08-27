;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package yasnippet
  :if (not (memq 'yasnippet grok-packages-disabled))
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package eglot
  :if (not (memq 'eglot grok-packages-disabled))
  :after yasnippet
  :ensure nil) ; prefer builtin

(use-package flymake
  :if (not (memq 'flymake grok-packages-disabled))
  :ensure nil ; prefer builtin
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package eldoc-box
  :if (not (memq 'eldoc-box grok-packages-disabled))
  :ensure t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
  ;; consider +  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :init (setq eldoc-display-functions (delq #'eldoc-display-in-echo-area eldoc-display-functions))
  :config
  (setq
   eldoc-idle-delay 0.0
   eldoc-box-cleanup-interval 0.0
   eldoc-box-only-multi-line nil
   eldoc-box-max-pixel-width 640
   eldoc-box-max-pixel-height 480
   eldoc-box-fringe-use-same-bg t))

(use-package sideline
  :if (not (memq 'sideline grok-packages-disabled))
  :ensure t
  :hook ((prog-mode . sideline-mode))
  :custom
  (sideline-backends-right '(sideline-flymake)))

(use-package sideline-flymake
  :if (not (memq 'sideline-flymake grok-packages-disabled))
  :ensure t :after sideline)

(use-package corfu
  :if (not (memq 'corfu grok-packages-disabled))
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)          ;; auto-show popup
  (corfu-auto-delay 0)    ;; no delay
  (corfu-auto-prefix 3)   ;; start after 3 chars
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator))

;; A few more useful configurations...
(use-package emacs
  :if (not (memq 'corfu-mode-grok-config grok-packages-disabled))
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(provide 'grok-lsp)
