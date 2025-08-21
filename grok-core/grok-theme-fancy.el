;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;----- fancy theme settings

(use-package emacs
  :ensure nil
  :preface
  (defvar grok-window-pop-enabled t
    "If non-nil, enable window resize and auto treemacs pop on startup.")

  ;; trying to achieve a wider visual-studio style window setup
  (defun grok-fancy-setup ()
    (set-frame-size (selected-frame) 135 40)
    (treemacs)
    (switch-to-buffer "*dashboard*"))

  :init
  (when (and grok-window-pop-enabled
             (string= grok-theme-style "fancy"))
    (add-hook 'window-setup-hook #'grok-fancy-setup)))

(use-package dashboard
  :if (string= grok-theme-style "fancy")
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
        dashboard-projects-backend 'projectile
        dashboard-projects-switch-function #'projectile-switch-project)

  (setq dashboard-navigator-buttons
        `(((nil "Homepage" "Go to project homepage"
                (lambda (&rest _) (browse-url "https://github.com/a-schaefers/grokEmacs")))

           (nil "grok.el Info" "Show Keybinds"
                (lambda (&rest _) (grok-show-keybinds)))

           (nil "Restart"  "Restart Emacs"
                (lambda (&rest _)
                  (restart-emacs)))))))

(use-package doom-themes
  :if (string= grok-theme-style "fancy")
  :ensure t
  :init (defun grok/apply-doom-theme () (require 'doom-themes) (load-theme (if (string= grok-theme-mode "light") 'doom-one-light 'doom-one) t) (global-hl-line-mode 1))
  :hook (elpaca-after-init . grok/apply-doom-theme))

(use-package doom-modeline
  :if (string= grok-theme-style "fancy")
  :ensure t
  :init (defun grok/enable-doom-modeline () (require 'doom-modeline) (doom-modeline-mode 1))
  :hook (elpaca-after-init . grok/enable-doom-modeline)
  :custom
  (doom-modeline-minor-modes t) ;; use minions instead
  (doom-modeline-height 32)
  (doom-modeline-bar-width 4))

(use-package minions :ensure t :after doom-modeline :init (minions-mode))

(use-package anzu
  :ensure t
  :after doom-modeline
  :init
  (global-anzu-mode 1)
  :custom (anzu-mode-lighter ""))

(use-package treemacs
  :if (string= grok-theme-style "fancy")
  :ensure t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config

  (defun grok/treemacs-darken-background (&rest _)
    "Darken Treemacs background relative to current theme."
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
        ("C-t"       . treemacs)

        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :if (string= grok-theme-style "fancy")
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :if (string= grok-theme-style "fancy")
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :if (string= grok-theme-style "fancy")
  :after (treemacs magit)
  :ensure t)

(use-package emacs
  :ensure nil
  ;; :bind (("C-h C-g" . grok-show-keybinds))
  :preface
  ;; autogenerated shitshow by ChatGPT "5 Thinking" (ツ)_/¯
  (defun grok-insert-eglot-server-programs (table)
    "Insert which eglot servers from TABLE are found on PATH.
TABLE is like `eglot-server-programs'."
    (when (and (boundp table) (symbol-value table))
      (dolist (entry (symbol-value table))
        (let* ((modes (car entry))
               (spec  (cdr entry))
               ;; normalize spec to a list-of-strings command if possible
               (cmd   (cond
                       ;; spec is a string -> wrap in list
                       ((stringp spec) (list spec))
                       ;; spec is a list of strings -> use directly
                       ((and (listp spec)
                             (cl-every #'stringp spec))
                        spec)
                       ;; spec is a dotted pair . <function> -> skip
                       ((and (consp spec)
                             (functionp (cdr spec))) nil)
                       (t spec))))
          (when (and (listp cmd)
                     (stringp (car cmd))
                     (executable-find (car cmd)))
            (insert (format "  %-40s -> %s\n"
                            (if (listp modes) (prin1-to-string modes) (symbol-name modes))
                            (mapconcat #'identity cmd " "))))))))
  (defun grok-insert-eglot-servers ()
    "Insert available Eglot servers from `grok-eglot-autostart-langs`.
Only runs if `grok-eglot-autostart-langs` is bound and non-nil."
    (when (bound-and-true-p grok-eglot-autostart-langs)
      (dolist (pair grok-eglot-autostart-langs)
        (let* ((hook (car pair))
               (spec (cdr pair))
               ;; unwrap (:override "cmd") etc.
               (cmd (cond
                     ((stringp spec) spec)
                     ((and (consp spec) (eq (car spec) :override))
                      (cadr spec))
                     (t nil))))
          (when (and cmd (executable-find cmd))
            (insert (format "  %-22s -> %s\n" hook cmd)))))))

  (defun grok-show-keybinds ()
    "Show Grok Emacs keybinds full-frame; press q to close.
Scans only `grokel` (or ~/.emacs.d/grok.el) for :bind."
    (interactive)
    (let* ((file (if (and (boundp 'grokel) grokel)
                     grokel
                   (expand-file-name "grok.el" user-emacs-directory)))
           (buf  (get-buffer-create "*Grok Keybinds*"))
           rows)
      ;; Parse grok.el, collect (MAP KEY CMD) from :bind only
      (when (and file (file-readable-p file))
        (let* ((content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string)))
               (pos 0)
               res obj)
          (while (< pos (length content))
            (setq res (ignore-errors (read-from-string content pos)))
            (if (not res)
                (setq pos (length content))
              (setq obj (car res)
                    pos (cdr res))
              (when (and (consp obj) (eq (car obj) 'use-package))
                ;; Walk the arglist as a stream of keywords; each keyword consumes
                ;; all following non-keyword forms. This way multiple forms under
                ;; :init (etc.) don't break finding :bind.
                (let ((tail (cddr obj)))
                  (while tail
                    (let ((item (car tail)))
                      (setq tail (cdr tail))
                      (when (keywordp item)
                        (let ((kw item) (acc nil))
                          (while (and tail (not (keywordp (car tail))))
                            (push (car tail) acc)
                            (setq tail (cdr tail)))
                          (when (eq kw :bind)
                            (dolist (valform (nreverse acc))
                              (let ((lst (if (and (consp valform)
                                                  (or (stringp (car valform))
                                                      (eq (car valform) :map)
                                                      (consp (car valform))))
                                             valform
                                           (list valform)))
                                    (curm 'global))
                                (while lst
                                  (let ((x (car lst)))
                                    (setq lst (cdr lst))
                                    (cond
                                     ;; Flat plist style: :map MAP
                                     ((eq x :map)
                                      (when lst
                                        (setq curm (car lst))
                                        (setq lst (cdr lst))))
                                     ;; Grouped style: (:map MAP ...bindings...)
                                     ((and (consp x) (eq (car x) :map))
                                      (setq curm (cadr x))
                                      (setq lst (append (cddr x) lst)))
                                     ;; Binding: ("KEY" . cmd) / ("KEY" cmd) / ("KEY" #'cmd) / ("KEY" 'cmd)
                                     ((and (consp x) (stringp (car x)))
                                      (let* ((key (car x))
                                             (rhs (cdr x))
                                             (cmd (cond
                                                   ((symbolp rhs) rhs)
                                                   ((and (consp rhs) (symbolp (car rhs))) (car rhs))
                                                   ((and (consp rhs) (eq (car rhs) 'function)
                                                         (symbolp (cadr rhs))) (cadr rhs))
                                                   ((and (consp rhs) (eq (car rhs) 'quote)
                                                         (symbolp (cadr rhs))) (cadr rhs))
                                                   (t nil))))
                                        (when (and key cmd)
                                          (push (list curm key cmd) rows))))
                                     ;; Nested list -> flatten and continue
                                     ((consp x)
                                      (setq lst (append x lst)))
                                     ;; else ignore
                                     )))))))))))))))

        ;; Render buffer
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert (propertize
         "\nBinds found in grok.el - Use M-x `grok-edit-grok-file' to modify\n\n"
         'face 'success))
          (if rows
              (progn
                (setq rows
                      (sort rows
                            (lambda (a b)
                              (let* ((ma (symbol-name (or (nth 0 a) 'global)))
                                     (mb (symbol-name (or (nth 0 b) 'global)))
                                     (ka (nth 1 a))
                                     (kb (nth 1 b)))
                                (if (string= ma mb)
                                    (string< ka kb)
                                  (string< ma mb))))))
                (let ((curm nil))
                  (dolist (r rows)
                    (let ((m   (or (nth 0 r) 'global))
                          (key (nth 1 r))
                          (cmd (nth 2 r)))
                      (unless (eq m curm)
                        (setq curm m)
                        (insert (format "[%s]\n" (if (eq m 'global) "global" (symbol-name m)))))
                      (insert (format "  %-16s -> %s\n" key cmd))))))
            (insert (format "No :bind forms found in %s\n" file)))
          (insert "\n")
          (when (bound-and-true-p grok-env)
            (when (bound-and-true-p grok-env)
  (insert (concat (propertize "env modifications: " 'face 'success)
                  (format "%S" grok-env))))
            (insert "\n\n")
            (when (getenv "PATH")
  (insert (concat (propertize "PATH: " 'face 'success)
                  (format "%S" (getenv "PATH"))))))
          (when (bound-and-true-p grok-eglot-autostart-langs)
            (insert "\n\n")
            (insert (propertize "LSP servers found on PATH and configured to auto-start:\n"
                    'face 'success))
            (grok-insert-eglot-servers))
          (require 'eglot)
          (when (bound-and-true-p eglot-server-programs)
            (insert "\n")
            (insert (propertize "ALL LSP servers available on PATH:\n"
                    'face 'success))   ;; green-ish, theme-safe
            (grok-insert-eglot-server-programs 'eglot-server-programs))
          ;; (insert (propertize "🧙 Grok Wizardry\n" 'face 'success))
          (goto-char (point-min))
          (special-mode)
          (setq-local header-line-format (propertize "🧙 Grok Diagnostics 🐊✨" 'face 'success)))
        (switch-to-buffer buf)
        (delete-other-windows)))))

(provide 'grok-theme-fancy)
