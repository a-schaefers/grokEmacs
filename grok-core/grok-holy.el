;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package crux
  :if (not (bound-and-true-p grok-evil))
  :ensure t
  :init
  (global-set-key (kbd "C-<tab>") 'crux-other-window-or-switch-buffer)
  (global-set-key (kbd "C-x C-o") 'crux-other-window-or-switch-buffer)
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-o") 'crux-smart-open-line)
  (global-set-key (kbd "C-c l") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-S-k") 'crux-kill-whole-line)
  (global-set-key (kbd "C-c C-;") 'crux-duplicate-and-comment-current-line-or-region))

(use-package paredit
   :if (not (bound-and-true-p grok-evil))
   :ensure t
   :init
   (add-hook 'emacs-lisp-mode-hook        #'enable-paredit-mode)
   (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
   (add-hook 'ielm-mode-hook               #'enable-paredit-mode)
   ;; lisps
   (add-hook 'lisp-interaction-mode-hook   #'enable-paredit-mode)
   (add-hook 'lisp-mode-hook               #'enable-paredit-mode)
   ;; schemes
   (add-hook 'scheme-mode-hook             #'enable-paredit-mode)
   ;; clojure
   (with-eval-after-load 'clojure-mode
     (add-hook 'clojure-mode-hook          #'enable-paredit-mode))
   ;; racket
   (with-eval-after-load 'racket-mode
     (add-hook 'racket-mode-hook           #'enable-paredit-mode)))

(provide 'grok-holy)
