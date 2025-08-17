;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package evil
  :if (bound-and-true-p grok-evil-mode)
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config (evil-mode 1))


(use-package evil-collection
  ;; so Magit, dired, etc. feel vimmy
  :after evil
  :ensure t
  :demand t
  :config
  (evil-collection-init))

(use-package evil-commentary
  ;; comments
  :if (bound-and-true-p grok-evil-mode)
  :ensure t
  :demand t
  :after (evil)
  :config (evil-commentary-mode 1))

(use-package evil-surround
  ;; surround editing
  :ensure t
  :demand t
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  ;; jump between pairs
  :ensure t
  :demand t
  :config
  (global-evil-matchit-mode 1))

(use-package smartparens
  ;; dep for evil-cleverparens
  :if (bound-and-true-p grok-evil-mode)
  :ensure t
  :demand t)

(use-package evil-cleverparens
  :if (bound-and-true-p grok-evil-mode)
  ;; evil mode's cousin to paredit
  :ensure t
  :demand t
  :after (evil smartparens))

;; for reasons unknown, these do not get registered within the use-package macro -- tried :command, :hook: :init :config everything i could think of
;; paredit does not have this issue. some kind of unique elpaca + use-package + evil-cleverparens issue
(add-hook 'emacs-lisp-mode-hook        #'evil-cleverparens-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'evil-cleverparens-mode)
(add-hook 'ielm-mode-hook               #'evil-cleverparens-mode)
(add-hook 'lisp-interaction-mode-hook   #'evil-cleverparens-mode)
(add-hook 'lisp-mode-hook               #'evil-cleverparens-mode)
(add-hook 'scheme-mode-hook             #'evil-cleverparens-mode)
(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode))
(with-eval-after-load 'racket-mode
  (add-hook 'racket-mode-hook  #'evil-cleverparens-mode))

(provide 'grok-evil)
