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

(use-package company
  :if (not (memq 'company grok-packages-disabled))
  :ensure t
  :hook ((prog-mode . company-mode)
         (html-ts-mode . company-mode))
  :config
  (setq company-idle-delay 0))

(provide 'grok-lsp)
