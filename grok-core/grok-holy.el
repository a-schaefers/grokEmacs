;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package crux
  :if (not (memq 'crux grok-packages-disabled))
  :ensure t)

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

;; Below is an arrangement which activates a which-key helper menu on M-m, but only on M-m--
;; without technically spamming which-key noise everywhere on all the other keys,
;; which is something that has caused me to previously avoid which-key for several years
;; This type of arrangement is not needed on something like evil-mode, where the user is not often doing things like:
;; C-x f C-x b C-x C-c C-c foo C-c bar etc etc. - it is those types of maneuvers that cause the which-key noise

(use-package which-key
  :if (not (memq 'which-key grok-packages-disabled))
  :ensure t
  :init
  (setq which-key-idle-delay 10000
        which-key-idle-secondary-delay 0.05
        which-key-show-early-on-C-h t
        which-key-use-C-h-commands t)
  :config
  (which-key-mode 1))

;; Leaders (global M-m) — prefix + instant which-key via C-h injection hack
(use-package general
  :if (not (memq 'general grok-packages-disabled))
  :preface
  (defun grok/recentf-vertico ()
    "Pick a file from `recentf-list' using `completing-read'."
    (interactive)
    (unless (bound-and-true-p recentf-mode)
      (user-error "recentf-mode is not enabled"))
    (find-file
     (completing-read "Recent file: " recentf-list nil t)))
  :ensure t
  :demand t
  :config
  ;; Ensure the override map is active everywhere
  (when (fboundp 'general-override-mode)
    (general-override-mode 1))

  ;; Real leader keymap (prefix)
  (define-prefix-command 'grok/leader-map)

  ;; Sub-prefix maps (real keymaps, not commands)
  (define-prefix-command 'grok/leader-files-map)
  (define-prefix-command 'grok/leader-projectile-map)
  (define-prefix-command 'grok/leader-shells-map)
  (define-prefix-command 'grok/leader-buffers-map)
  (define-prefix-command 'grok/leader-config-map)
  (define-prefix-command 'grok/leader-windows-map)

  ;; Attach submaps to section keys
  (define-key grok/leader-map (kbd "f") 'grok/leader-files-map)
  (define-key grok/leader-map (kbd "p") 'grok/leader-projectile-map)
  (define-key grok/leader-map (kbd "c") 'grok/leader-config-map)
  (define-key grok/leader-map (kbd "s") 'grok/leader-shells-map)
  (define-key grok/leader-map (kbd "b") 'grok/leader-buffers-map)
  (define-key grok/leader-map (kbd "w") 'grok/leader-windows-map)

  ;; Labels for section keys in which-key
  (when (fboundp 'which-key-add-keymap-based-replacements)
    (which-key-add-keymap-based-replacements grok/leader-map
      "f" "files" "s" "shells"  "b" "buffers" "c" "config" "p" "projectile"  "w" "windows"))

  ;; keep M-m a prefix in general-override-map - Emacs will merge it with mode maps,
  ;; e.g. eglot-mode-map in grok.el will also display - and which-key will show both sets.
  (define-key general-override-mode-map (kbd "M-m") 'grok/leader-map)

  (define-key global-map                 (kbd "M-m") 'grok/leader-map)

  ;; Translate first press of M-m -> "M-m C-h" (show which-key instantly)
  ;; Hack to get the submenus to pop under the restricted M-m *only* which-key
  (defvar grok/leader--injecting-help nil)
  (defun grok/translate-M-m (_prompt)
    (if grok/leader--injecting-help
        (prog1 (kbd "M-m") (setq grok/leader--injecting-help nil))
      (setq grok/leader--injecting-help t)
      (kbd "M-m C-h")))
  (define-key key-translation-map (kbd "M-m") #'grok/translate-M-m)

  ;; Definer for top-level leaves on the leader map
  (general-create-definer grok/leader :keymaps 'grok/leader-map)

  ;; Top-level leaves (M-m g/t/p/d)
  (grok/leader
   "g"  '(magit-status             :which-key))

  (when (require 'treemacs nil 'noerror)
    (grok/leader
      "t"  '(treemacs                 :which-key)))

  ;; Files submenu (M-m f)
  (general-def :keymaps 'grok/leader-files-map
    "f" '(find-file          :which-key)
    "d" '(dired              :which-key)
    "e" '(ediff-files        :which-key)
    "s" '(save-buffer        :which-key)
    "r" '(grok/recentf-vertico :which-key))

  ;; Config submenu (M-m f)
  (general-def :keymaps 'grok/leader-config-map
    "g" '(grok-edit-grok-file               :which-key)
    "i" '(grok-edit-init-file               :which-key)
    "o" '(grok-edit-grok-initial-setup-opts :which-key)
    "u" '(grok-update-config-with-ediff     :which-key))

  ;; Projectile submenu (M-m p)
  (general-def :keymaps 'grok/leader-projectile-map
    "p" '(projectile-commander          :which-key)
    "f" '(projectile-find-file          :which-key)
    "g" '(projectile-grep               :which-key)
    "s" '(projectile-switch-project     :which-key))

  (when (executable-find "rg")
    (general-def :keymaps 'grok/leader-projectile-map
      "r" '(projectile-ripgrep            :which-key)))

  ;; Shells submenu (M-m s)
  (general-def :keymaps 'grok/leader-shells-map
    "s" '(shell           :which-key)
    "a" '(ansi-term       :which-key)
    "e" '(eshell          :which-key))
  (when (require 'eat nil 'noerror)
    (general-def :keymaps 'grok/leader-shells-map
      "h" '(eat :which-key)))
  (when (require 'vterm nil 'noerror)
    (general-def :keymaps 'grok/leader-shells-map
      "v" '(vterm :which-key)))

  ;; Buffers submenu (M-m b)
  (general-def :keymaps 'grok/leader-buffers-map
    "b" '(switch-to-buffer   :which-key)
    "k" '(kill-buffer        :which-key))

  ;; Windows submenu (M-m w)
  (general-def :keymaps 'grok/leader-windows-map
    "w" '(other-window         :which-key)
    "v" '(split-window-right   :which-key)
    "s" '(split-window-below   :which-key)
    "d" '(delete-window        :which-key)
    "o" '(delete-other-windows :which-key)))

(provide 'grok-holy)
