;;; init.el --- Initialization file for Emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;;
;; Grok Emacs goes out of its way to `provide' & `require' all .el files, avoiding load and load-file,
;; because initially those other methods seemed to have odd behavior under Elpaca. Currently, the author is unsure.
;;
;; It also goes out of its way to `use-package' enclose everything throughout as much as possible, to nigh absurdity.
;; If for nothing else, it begins to standardize a way to do configuration in a language (elisp) where there are
;; many ways to accomplish the same end. The use-package built-in is great for this, as most Emacs users globally are already
;; familiar with it, and we thus avoid creating yet more macros and wrappers around use-package and "framework-lockin".
;;
;; This file:
;;
;; Sets load-path, requires grok-bootstrap, and then requires grok-core modules.
;; Conditionally enables grok-holy or grok-evil and grok-theme-minimal or grok-theme-fancy.
;; Ensures grok.d exists, seeds grok.d/grok.el from grok-defaults.el, and provides edit/ediff helpers.
;; Loads every *.el in grok.d, configures Eglot autostart from grok-eglot-autostart-langs,
;; then drains Elpaca’s queue with elpaca-wait for deterministic startup.

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

(setq grokfile (expand-file-name "grok-defaults.el" user-emacs-directory)
      grokd    (expand-file-name "grok.d" user-emacs-directory)
      grokel   (file-name-concat grokd "grok.el"))

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

(use-package emacs
  :ensure nil
  :init
  (require 'subr-x)
  (when (boundp 'grok-eglot-autostart-langs)
    (dolist (pair grok-eglot-autostart-langs)
      (let* ((hook (car pair))
             (val (cdr pair))
             (mode (intern (string-remove-suffix "-hook" (symbol-name hook))))
             (override (and (consp val) (eq (car val) :override)))
             (lsp-bin (if override (cadr val) val))
             (cmd (cond ((symbolp lsp-bin) (list (symbol-name lsp-bin)))
                        ((listp  lsp-bin) lsp-bin)
                        (t nil))))
        (when (and cmd (executable-find (car cmd)))
          (add-hook hook #'eglot-ensure))
        (when override
          (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs (cons mode cmd))))))))

;; block until currently queued orders are processed.

(elpaca-wait)

;;; init.el ends here
