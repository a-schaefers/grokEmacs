;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(add-to-list 'load-path (expand-file-name "grok-core" user-emacs-directory))

;; C-u M-x grok--ensure-opts will reconfigure
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
             grok-theme-globals
             ,@(when (string= grok-theme-style "minimal") '(grok-theme-minimal))
             ,@(when (string= grok-theme-style "fancy") '(grok-theme-fancy))
             ))
  (require f))

;; generate grok configuration file

(setq grokd (expand-file-name "grok.d" user-emacs-directory)
      grokel (concat grokd "/" "grok.el")
      grokfile (concat user-emacs-directory "grok-defaults.el"))

(or (file-exists-p grokel)
    (copy-file grokfile grokel))

(defun grok-update-config-with-ediff ()
  (interactive)
  (ediff-files grokel grokfile))

(defun grok-edit-init-file ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun grok-edit-grok-file ()
  (interactive)
  (find-file grokel))

(defun grok-edit-grok-initial-setup-opts ()
  (interactive)
  (find-file grok-opts-file))

;; grok.d/**

(unless (file-directory-p grokd)
  (make-directory grokd))

(add-to-list 'load-path grokd)

(dolist (file (directory-files grokd nil "\\.el\\'"))
  (require (intern (file-name-base file))))

;; configure eglot autostart hooks for specified language modes.

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
