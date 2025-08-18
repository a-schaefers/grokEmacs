;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package evil
  :if (bound-and-true-p grok-evil-mode)
  :ensure t
  :hook (elpaca-after-init . evil-mode)
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil))

(use-package evil-collection
  ;; so Magit, dired, etc. feel vimmy
  :if (bound-and-true-p grok-evil-mode)
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-commentary
  ;; comments
  :if (bound-and-true-p grok-evil-mode)
  :ensure t
  :after evil
  :config (evil-commentary-mode 1))

(use-package evil-surround
  ;; surround editing
  :if (bound-and-true-p grok-evil-mode)
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  ;; jump between pairs
  :if (bound-and-true-p grok-evil-mode)
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package smartparens
  ;; dep for evil-cleverparens
  :if (bound-and-true-p grok-evil-mode)
  :after evil
  :ensure t)

(use-package evil-cleverparens
  ;; evil mode's cousin to paredit
  :if (bound-and-true-p grok-evil-mode)
  :ensure t
  :after (evil smartparens))

;; for reasons unknown, these do not get registered within the above evil-cleverparens use-package declaration
;; tried :command, :hook: :init :config
;; paredit does not have this issue.
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

(use-package treemacs-evil
  :if (and (bound-and-true-p grok-evil-mode) (string= grok-theme-style "fancy"))
  :after (treemacs evil)
  :ensure t)

;; example for vimmers who want to setup a leader on the Space bar

;; (use-package which-key
;;   :if (bound-and-true-p grok-evil-mode)
;;   :ensure t
;;   :after evil
;;   :config
;;   (setq which-key-idle-delay 0.0)
;;   (which-key-mode 1))

;; ;; Leaders (global SPC, local SPC m …)
;; (use-package general
;;   :if (bound-and-true-p grok-evil-mode)
;;   :after evil
;;   :ensure t
;;   :config
;;   ;; Global leader: SPC …
;;   (general-create-definer grok/leader
;;                           :states '(normal visual motion)
;;                           :prefix "SPC"
;;                           :global-prefix "C-SPC")

;;   ;; Local/holy leader: SPC m … (per-mode menus)
;;   (general-create-definer grok/local-leader
;;                           :states '(normal visual motion)
;;                           :prefix "SPC m"
;;                           :global-prefix "C-SPC m")

;;   ;; ----- Global SPC bindings -----
;;   (grok/leader
;;    ;; files
;;    "f"  '(:ignore t :which-key "files")
;;    "ff" '(find-file            :which-key "find file")    ;; C-x C-f
;;    "fd" '(dired                :which-key "dired")        ;; C-x d
;;    "fs" '(save-buffer          :which-key "save file")    ;; C-x C-s
;;    "fr" '(recentf-open-files   :which-key "recent files")

;;    ;; buffers
;;    "b"  '(:ignore t :which-key "buffers")
;;    "bb" '(switch-to-buffer     :which-key "switch buffer") ;; C-x b
;;    "bk" '(kill-buffer          :which-key "kill buffer")   ;; C-x k

;;    ;; windows
;;    "w"  '(:ignore t :which-key "windows")
;;    "ww" '(other-window   :which-key "other window")        ;; C-x o
;;    "wv" '(split-window-right   :which-key "vsplit")        ;; C-x 3
;;    "ws" '(split-window-below   :which-key "hsplit")        ;; C-x 2
;;    "wd" '(delete-window        :which-key "del window")    ;; C-x 0
;;    "wo" '(delete-other-windows :which-key "only window")   ;; C-x 1

;;    "g" '(magit-status         :which-key "magit")
;;    "t" '(treemacs             :which-key "treemacs")
;;    "p" '(projectile-commander         :which-key "projectile")
;;    "d" '(eldoc-box-help-at-point :which-key "eldoc at point")))

(provide 'grok-evil)
