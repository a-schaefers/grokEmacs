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

(dolist (file (directory-files grokd nil "\\.el\\'"))
  (require (intern (file-name-base file))))

;; block until currently queued orders are processed.

(elpaca-wait)

;;; init.el ends here
