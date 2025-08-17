;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package prescient :ensure t :demand t)

(use-package vertico
  :ensure t
  :demand t
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (vertico-mode 1)
  (savehist-mode 1)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

(use-package vertico-prescient
  :ensure t
  :demand t
  :after (vertico prescient)
  :config
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1))

(provide 'grok-vertico)
