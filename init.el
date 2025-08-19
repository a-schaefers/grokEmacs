;;; init.el --- Initialization file for Emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Entry point for grokEmacs: sets `load-path`, loads bootstrap, then requires core modules.
;; Conditionally enables Holy/Evil and Minimal/Fancy themes via `grok-evil-mode` / `grok-theme-style`.
;; Ensures ~/.config/emacs/grok.d exists, seeds grok.d/grok.el from grok-defaults.el, and provides edit/ediff helpers.
;; Loads every *.el in grok.d, configures Eglot autostart from `my-eglot-autostart-langs` (per-mode overrides supported),
;; then drains Elpaca’s queue with `elpaca-wait` for deterministic startup.

;;; Code:

(add-to-list 'load-path (expand-file-name "grok-core" user-emacs-directory))

(require 'grok-bootstrap)

(dolist (f `(
             grok-elpaca
             grok-better-defaults
             grok-better-scratch
             grok-vertico
             ,@(unless grok-evil-mode '(grok-holy))
             ,@(when grok-evil-mode '(grok-evil))
             grok-lsp
             grok-magit
             grok-projectile
             grok-terminal
             grok-treesit
             ,@(when (member grok-theme-style '("minimal" "fancy")) '(grok-theme-shared))
             ,@(when (member grok-theme-style '("minimal")) '(grok-theme-minimal))
             ,@(when (member grok-theme-style '("fancy")) '(grok-theme-fancy))
             ))
  (require f))

;; grok.d/ & grok.el

(setq grokd (expand-file-name "grok.d" user-emacs-directory)
      grokel (concat grokd "/" "grok.el")
      grokfile (concat user-emacs-directory "grok-defaults.el"))

(unless (file-directory-p grokd)
  (make-directory grokd))

(add-to-list 'load-path grokd)

(unless (file-exists-p grokel)
    (copy-file grokfile grokel))

(defun grok-update-config-with-ediff ()
  "Ediff your grok.el against grok-defaults.el.  Useful after a 'git pull' in case of a breaking upstream change."
  (interactive)
  (ediff-files grokel grokfile))

(defun grok-edit-init-file ()
  "Open init.el."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun grok-edit-grok-file ()
  "Open grok.el."
  (interactive)
  (find-file grokel))

(defun grok-edit-grok-initial-setup-opts ()
  "Open grok-opts.el."
  (interactive)
  (find-file grok-opts-file))

(dolist (file (directory-files grokd nil "\\.el\\'"))
  (require (intern (file-name-base file))))

;; configure eglot per `grok-eglot-autostart-langs'

(dolist (pair my-eglot-autostart-langs)
    (let* ((hook (car pair))
           (val (cdr pair))
           (mode (intern (string-remove-suffix "-hook" (symbol-name hook))))
           (override (and (consp val) (eq (car val) :override)))
           (lsp-bin (if override (cadr val) val))
           (cmd (cond
                 ((symbolp lsp-bin) (list (symbol-name lsp-bin)))
                 ((listp lsp-bin) lsp-bin)
                 (t nil))))
      (when (and cmd (executable-find (car cmd)))
        (add-hook hook #'eglot-ensure))
      (when override
        (eval-after-load 'eglot
          `(add-to-list 'eglot-server-programs
                        '(,mode . ,cmd))))))

;; block until currently queued orders are processed.

(elpaca-wait)

;;; init.el ends here
