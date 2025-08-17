;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package yasnippet
  :ensure t
  :demand t
  :hook (prog-mode-hook . yas-minor-mode))

(use-package eglot
  :after yasnippet
  :ensure t
  :demand t)

(use-package flymake
  :ensure t
  :demand t
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package eldoc-box
  :ensure t
  :demand t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
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
  :demand t
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'html-ts-mode-hook 'company-mode) ; apparently not a prog-mode

  (setq company-idle-delay 0))

(use-package company-prescient
  :after (company prescient)
  :ensure t
  :demand t
  :config (company-prescient-mode 1))


(provide 'grok-lsp)
