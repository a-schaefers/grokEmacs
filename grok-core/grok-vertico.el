;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package vertico
  :ensure t
  :init (vertico-mode 1))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
         ("RET"   . vertico-directory-enter)
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

(use-package orderless
  :ensure t
  :init
  ;; Order: Vertico UI → Orderless filter → fall back to built-ins (basic, flex).
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)
        completion-styles '(orderless basic flex)
        completion-category-defaults nil
        ;; Files/TRAMP: keep basic & partial-completion first; still allow orderless/flex.
        completion-category-overrides '((file (styles basic partial-completion orderless flex)))))

(use-package emacs
  :ensure nil
  :init
  (context-menu-mode 1)
  (setq enable-recursive-minibuffers t
        read-extended-command-predicate #'command-completion-default-include-p
        minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(provide 'grok-vertico)
