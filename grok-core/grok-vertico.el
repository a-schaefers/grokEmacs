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
  :custom
  ;; Completion styles: try prefix/exact first, then fall back to Orderless.
  ;; This makes non-LSP languages (e.g. Emacs Lisp) feel more relevant,
  ;; but still gives you fuzzy rescue when you need it.
  (completion-styles '(basic orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))

  ;; When Orderless runs, allow literal, regexp, and flex (fuzzy) matching.
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

(use-package emacs
  :ensure nil
  :init
  (context-menu-mode 1)
  (setq enable-recursive-minibuffers t
        read-extended-command-predicate #'command-completion-default-include-p
        minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(provide 'grok-vertico)
