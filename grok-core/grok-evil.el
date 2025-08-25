;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package evil
  :if (not (memq 'evil grok-packages-disabled))
  :ensure t
  :hook (elpaca-after-init . evil-mode)
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil))

(use-package evil-collection
  ;; so Magit, dired, etc. feel vimmy
  :if (not (memq 'evil-collection grok-packages-disabled))
  :after evil
  :ensure t
  :init
  (evil-collection-init))

(use-package evil-commentary
  ;; comments
  :if (not (memq 'evil-commentary grok-packages-disabled))
  :ensure t
  :after evil
  :init (evil-commentary-mode 1))

(use-package evil-surround
  ;; surround editing
  :if (not (memq 'evil-surround grok-packages-disabled))
  :ensure t
  :after evil
  :init
  (global-evil-surround-mode 1))

(use-package evil-matchit
  ;; jump between pairs
  :if (not (memq 'evil-matchit grok-packages-disabled))
  :ensure t
  :after evil
  :init
  (global-evil-matchit-mode 1))

(use-package smartparens
  ;; dep for evil-cleverparens
  :if (not (or (memq 'evil-cleverparens grok-packages-disabled)
               (memq 'smartparens grok-packages-disabled)))
  :after evil
  :ensure t

  :init
  (when (not (memq 'evil-cleverparens grok-packages-disabled))
    (add-hook 'emacs-lisp-mode-hook        #'evil-cleverparens-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'evil-cleverparens-mode)
    (add-hook 'ielm-mode-hook               #'evil-cleverparens-mode)
    (add-hook 'lisp-interaction-mode-hook   #'evil-cleverparens-mode)
    (add-hook 'lisp-mode-hook               #'evil-cleverparens-mode)
    (add-hook 'scheme-mode-hook             #'evil-cleverparens-mode)
    (with-eval-after-load 'clojure-mode
      (add-hook 'clojure-mode-hook #'evil-cleverparens-mode))
    (with-eval-after-load 'racket-mode
      (add-hook 'racket-mode-hook  #'evil-cleverparens-mode))))

(use-package evil-cleverparens
  ;; evil mode's cousin to paredit
  :if (not (or (memq 'evil-cleverparens grok-packages-disabled)
               (memq 'smartparens grok-packages-disabled)))
  :ensure t
  :after (evil smartparens))

(use-package treemacs-evil
  :if (and (bound-and-true-p grok-evil-mode)
           (string= grok-theme-style "fancy")
           (not (memq 'treemacs-evil grok-packages-disabled)))
  :after (treemacs evil)
  :ensure t)

;; Space leader key

(use-package which-key
  :if (not (memq 'which-key grok-packages-disabled))
  :ensure t
  :after evil
  :init
  (setq which-key-idle-delay 0.0)
  (which-key-mode 1))

;; Leaders (global SPC, local SPC m)
(use-package general
  :if (not (memq 'general grok-packages-disabled))
  :after evil
  :ensure t
  :init

  (general-create-definer grok/leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :prefix-map 'grok/leader-map)

  ;; Global SPC bindings
  (grok/leader
    ;; files
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file             :which-key "find file")
    "fd" '(dired                 :which-key "dired")
    "fe" '(ediff-files           :which-key "ediff files")
    "fs" '(save-buffer           :which-key "save file")
    "fr" '(grok/recentf-vertico  :which-key "recent files")

    ;; projectile
    "p"  '(:ignore t :which-key "projectile")
    "pp" '(projectile-commander      :which-key "commander")
    "pf" '(projectile-find-file      :which-key "find file")
    "pg" '(projectile-grep           :which-key "grep")
    "ps" '(projectile-switch-project :which-key "switch project")

    "c"  '(:ignore t :which-key "config")
    "cg" '(grok-edit-grok-file               :which-key "Edit grok.el")
    "ci" '(grok-edit-init-file               :which-key "Edit init.el")
    "co" '(grok-edit-grok-initial-setup-opts :which-key "Edit grok-opts.el")
    "cu" '(grok-update-config-with-ediff     :which-key "grok-update-config-with-ediff")

    ;; shells
    "s"   '(:ignore t  :which-key "shells")
    "ss"  '(shell      :which-key "shell")
    "sa"  '(ansi-term  :which-key "ansi-term")
    "se"  '(eshell     :which-key "eshell")

    ;; buffers
    "b"  '(:ignore t        :which-key "buffers")
    "bb" '(switch-to-buffer :which-key "switch buffer")
    "bk" '(kill-buffer :which-key "kill current buffer")

    ;; utils
    "u"  '(:ignore t        :which-key "utils")
    "ub" '(eww :which-key "eww")
    "ue" '(erc :which-key "erc")
    "ui" '(rcirc :which-key "rcirc")

    ;; windows
    "w"  '(:ignore t :which-key "windows")
    "ww" '(other-window        :which-key "other window")
    "wv" '(split-window-right  :which-key "vsplit")
    "ws" '(split-window-below  :which-key "hsplit")
    "wd" '(delete-window       :which-key "del window")
    "wo" '(delete-other-windows :which-key "only window")

    ;; top-level leaves
    "g" '(magit-status            :which-key "magit"))

  ;; Conditional leaves

  (when (locate-library "treemacs")
    (grok/leader "t" '(treemacs :which-key "treemacs")))

  (when (locate-library "wanderlust")
    (grok/leader "uw" '(wanderlust :which-key "wanderlust")))

  (when (file-exists-p "~/.gnus.el")
    (grok/leader "ug" '(gnus :which-key "gnus")))

  (when (locate-library "mu4e")
    (grok/leader "um" '(mu4e :which-key "mu4e")))

  (when (executable-find "rg")
    (grok/leader "pr" '(projectile-ripgrep :which-key "ripgrep")))

  (when (locate-library "eat")
    (grok/leader "sh" '(eat :which-key "eat"))) ; h as in 'hungry', because 'e' belongs to eshell

  (when (locate-library "vterm")
    (grok/leader "sv" '(vterm :which-key "vterm"))))

(provide 'grok-evil)
