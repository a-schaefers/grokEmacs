;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;----- fancy theme settings

(use-package emacs
  :if (not (memq 'theme-fancy-grok-config grok-packages-disabled))
  :preface
  (defun grok-fancy-setup ()
    ;; trying to achieve a wider visual-studio style window setup
    (set-frame-size (selected-frame) 135 40)
    (when (and (not (memq 'treemacs grok-packages-disabled))
               (fboundp 'treemacs))
      (treemacs))
    (when (get-buffer "*dashboard*")
      (switch-to-buffer "*dashboard*")))
  :ensure nil
  :init
  (when (and grok-window-pop-enabled
             (string= grok-theme-style "fancy"))
    (add-hook 'window-setup-hook #'grok-fancy-setup)
    (when (and
           (display-graphic-p)
           (bound-and-true-p grok-window-pop-splash))
      (add-hook 'window-setup-hook (lambda () (grok-splash-overlay-later 0.00)) 90))))

(use-package dashboard
  :if (not (memq 'dashboard grok-packages-disabled))
  :ensure t
  :hook ((elpaca-after-init . dashboard-initialize)
         (elpaca-after-init . dashboard-insert-startupify-lists)
         (elpaca-after-init . dashboard-setup-startup-hook))
  :init
  (setq inhibit-startup-screen t
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-startup-banner (expand-file-name "grokEmacs.txt" user-emacs-directory)
        dashboard-banner-logo-title "Emacs"
        dashboard-footer-messages '("do you even grok bro 🐊✨")
        dashboard-items '((recents . 5)
                          (projects . 5))
        dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer)
        dashboard-projects-backend 'projectile)

  (setq dashboard-navigator-buttons
        `(((nil "Homepage" "Go to project homepage"
                (lambda (&rest _) (browse-url "https://github.com/a-schaefers/grokEmacs")))

           (nil "Tutorial"  "Official Intertactive Emacs Tutorial"
                (lambda (&rest _)
                  (help-with-tutorial)))

           (nil "Tour" "Go to the official Emacs guided tour"
                (lambda (&rest _) (browse-url "https://www.gnu.org/software/emacs/tour/")))

           (nil "Manual"  "Official Emacs Manual"
                (lambda (&rest _)
                  (info-emacs-manual)))

           (nil "Elisp Intro" "Go to the official elisp intro page"
                (lambda (&rest _) (browse-url "https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html")))))))

(when (not (memq 'theme-fancy-grok-config grok-packages-disabled))
  (grok--use-pkg-programmatic
    (grok-custom-theme-pkg
     :init
     ((defun grok--apply-theme ()
        (require grok-custom-theme-pkg)
        (load-theme grok-custom-theme t)
        (global-hl-line-mode 1))
      (add-hook 'elpaca-after-init-hook #'grok--apply-theme)))))

(use-package doom-modeline
  :if (and (string= grok-use-modeline "doom")
           (not (memq 'doom-modeline grok-packages-disabled)))
  :ensure t
  :init (defun grok/enable-doom-modeline () (doom-modeline-mode 1))
  :hook (elpaca-after-init . grok/enable-doom-modeline)
  :custom
  (doom-modeline-minor-modes t) ;; use minions instead
  (doom-modeline-height 32)
  (doom-modeline-bar-width 4))

(use-package spaceline
  :if (and (string= grok-use-modeline "spaceline")
           (not (memq 'spaceline grok-packages-disabled)))
  :ensure t
  :init
  (defun grok/enable-spaceline ()
    (require 'spaceline-config)
    (spaceline-toggle-minor-modes-on)
    (spaceline-emacs-theme))
  :hook (elpaca-after-init . grok/enable-spaceline))

(use-package moody
  ;; note to reader, you may want to customize the color of moody also, possibly by throwing something like this in grok.el:
  ;; (defun my/theme-mods ()
  ;;   (set-face-attribute 'mode-line nil :background "#285577")
  ;;   (set-face-attribute 'mode-line-inactive nil :background "#333333"))
  ;; (add-hook 'window-setup-hook #'my/theme-mods)

  :if (and (string= grok-use-modeline "moody")
           (not (memq 'moody grok-packages-disabled)))
  :ensure t
  :preface
  (defun grok/moody-disable-box ()
    (set-face-attribute 'mode-line nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :box nil))
  :hook (window-setup . grok/moody-disable-box)
  :init
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; uncluttered minor modes across various modelines
(use-package minions
  :if (not (memq 'minions grok-packages-disabled))
  :ensure t
  :hook (elpaca-after-init . minions-mode))

(use-package treemacs
  :if (not (memq 'treemacs grok-packages-disabled))
  :ensure t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config

  (defun grok/treemacs-darken-background (&rest _)
    "Darken Treemacs background relative to current theme."
    (require 'color)
    (face-remap-add-relative
     'default
     `(:background ,(color-darken-name (face-background 'default nil t) 5))))

  (add-hook 'treemacs-mode-hook #'grok/treemacs-darken-background)

  (progn
    (setq treemacs-buffer-name-function            #'treemacs-default-buffer-name
          treemacs-buffer-name-prefix              " *Treemacs-Buffer-"
          treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :if (not (memq 'treemacs-projectile grok-packages-disabled))
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :if (not (memq 'treemacs-icons-dired grok-packages-disabled))
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :if (not (memq 'treemacs-magit grok-packages-disabled))
  :after (treemacs magit)
  :ensure t)

(provide 'grok-theme-fancy)
