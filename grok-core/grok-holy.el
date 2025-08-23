;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package crux
  :if (not (memq 'crux grok-packages-disabled))
  :ensure t
  :bind (("C-g"     . crux-keyboard-quit-dwim)
         ("C-<tab>" . crux-other-window-or-switch-buffer)
         ("C-x C-o" . crux-other-window-or-switch-buffer)
         ("C-a"     . crux-move-beginning-of-line)
         ("C-o"     . crux-smart-open-line)
         ("C-c l"   . crux-duplicate-current-line-or-region)
         ("C-S-k"   . crux-kill-whole-line)
         ("C-c C-;" . crux-duplicate-and-comment-current-line-or-region)))

(use-package paredit
   :if (not (memq 'paredit grok-packages-disabled))
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
