;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package yasnippet
  :ensure t
  :hook (prog-mode-hook . yas-minor-mode))

(use-package eglot
  :after yasnippet
  :ensure nil ; prefer builtin
  :init
  (define-key eglot-mode-map (kbd "M-m r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-m o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "M-m h") 'eldoc)
  (define-key eglot-mode-map (kbd "M-m =") 'eglot-format)
  (define-key eglot-mode-map (kbd "M-m ?") 'xref-find-references)
  (define-key eglot-mode-map (kbd "M-.")   'xref-find-definitions))

(use-package flymake
  :ensure nil ; prefer builtin
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
  :init
  (global-set-key (kbd "C-c C-h") 'eldoc-box-help-at-point)
  :config
  (setq
   eldoc-idle-delay 0.0
   eldoc-box-cleanup-interval 0.0
   eldoc-display-functions (delq #'eldoc-display-in-echo-area eldoc-display-functions)
   eldoc-box-only-multi-line nil
   eldoc-box-max-pixel-width 640
   eldoc-box-max-pixel-height 480
   eldoc-box-fringe-use-same-bg t))

(use-package company
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'html-ts-mode-hook 'company-mode) ; apparently not a prog-mode
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)

  (setq company-idle-delay 0))

(provide 'grok-lsp)
