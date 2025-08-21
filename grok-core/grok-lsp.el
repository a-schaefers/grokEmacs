;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package yasnippet
  :ensure t
  :hook (prog-mode-hook . yas-minor-mode))

(use-package eglot
  :after yasnippet
  :ensure nil ; prefer builtin
  )

(use-package flymake
  :ensure nil ; prefer builtin
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
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
  :ensure t
  :hook ((prog-mode . sideline-mode))
  :custom
  (sideline-backends-right '(sideline-flymake)))

(use-package sideline-flymake :ensure t :after sideline)

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)
         (html-ts-mode . company-mode))
  :bind (:map
         company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         :map
         company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0))

(provide 'grok-lsp)
