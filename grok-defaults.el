;;; grok.el --- Local customization -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; A .gitignore'd user custom file.
;; Removing  it and restarting Emacs will regenerate anew.
;; This file should be diffed against grok-defaults.el any time after 'git pull' (or similar)
;; via `grok-update-config-with-ediff' (C-c gu) to ensure no breakage.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'ENV' / 'PATH' customization
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil
  :preface
  (setq grok-env '((EDITOR . "emacsclient")
                   (VISUAL . "$EDITOR")
                   (PAGER  . "cat")))

  (setq grok-path-insert '(""))

  (setq grok-path-append '(""))
  :init
  (grok-apply-env-and-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'LSP' - Modes that will autostart the corresponding server if found on PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil
  :preface
  (setq grok-eglot-autostart-langs
        '((c-ts-mode-hook          . "clangd")
          (c++-ts-mode-hook        . "clangd")
          (lua-ts-mode-hook        . "lua-language-server")
          (bash-ts-mode-hook       . "bash-language-server")
          (python-ts-mode-hook     . "pylsp")
          (go-ts-mode-hook         . "gopls")
          (rust-ts-mode-hook       . "rust-analyzer")
          (ruby-ts-mode-hook       . "solargraph")
          (elixir-ts-mode-hook     . (:override "elixir-ls"))
          (html-ts-mode-hook       . "vscode-html-language-server")
          (css-ts-mode-hook        . "vscode-css-language-server")
          (typescript-ts-mode-hook . "typescript-language-server")
          (js-ts-mode-hook         . "typescript-language-server")
          (yaml-ts-mode-hook       . "yaml-language-server")
          (json-ts-mode-hook       . "vscode-json-languageserver")
          (java-ts-mode-hook       . "jdtls")
          (csharp-ts-mode-hook     . "OmniSharp")


          ;; Additional commonly used modes that do not yet have a ts mode built-in at this time
          ;;
          ;; (markdown-mode-hook . marksman)
          ;; (php-mode-hook . true)          ; workaround, php lang server is not available on PATH but via required lib
          ;; (zig-mode-hook . zigls)
          ;; (terraform-mode-hook . terraform-ls)
          ;; (nix-mode-hook . rnix-lsp)
          ;; (haskell-mode-hook . haskell-language-server-wrapper)
          ;; (ocaml-mode-hook . ocaml-lsp)
          ;; (scala-mode-hook . metals)
          ;; (forth-mode-hook . forth-lsp)
          ;; (erlang-mode-hook . erlang_ls)
          ;; (racket-mode-hook . true)       ; workaround, racket lang server is not available on PATH but via required lib
          ;; (clojure-mode-hook . clojure-lsp)
          ))
          :init
          (grok-apply-eglot-autostart))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'General'
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :init
  ;; Python friendly
  (setq-default indent-tabs-mode nil
                fill-column 79)

  ;; Set default compile command, for M-x cc
  (setq compile-command "make -k ")

  ;; Some global binds

  :bind (:map global-map
              ("C-h C-b" . grok-show-keybinds)
              ("C-c gi"  . grok-edit-init-file)
              ("C-c gg"  . grok-edit-grok-file)
              ("C-c go"  . grok-edit-grok-initial-setup-opts)
              ("C-c gu"  . grok-update-config-with-ediff))
  :config
  ;; Some helpful aliases

  ;; M-x cc
  (defalias 'cc 'compile)

  ;; M-x lint
  (defalias 'lint 'grok-pop-flymake-diagnostics)

  ;; M-x git
  (defalias 'git 'magit)

  ;; M-x pro
  (defalias 'pro 'projectile-commander)

  ;; M-x sh (eat)
  (defalias 'sh 'eat)

  ;;  To setup shell integration for GNU Bash, put the following at the end of your .bashrc:
  ;; [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  ;;   source "$EAT_SHELL_INTEGRATION_DIR/bash"

  ;; For Zsh, put the following in your .zshrc:
  ;; [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  ;;   source "$EAT_SHELL_INTEGRATION_DIR/zsh"

  ;; Start Emacs server
  (require 'server)
  (unless (server-running-p) (server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'Packages'
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'Overrides' - hack on some already provided package by core, just be sure to add :ensure nil

(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("M-m r" . eglot-rename)
              ("M-m o" . eglot-code-action-organize-imports)
              ("M-m h" . eldoc)
              ("M-m =" . eglot-format)
              ("M-m ?" . xref-find-references)
              ("M-."   . xref-find-definitions)
              ("M-m d"   . eldoc-box-help-at-point)
              ("C-c C-h" . eldoc-box-help-at-point)))

(use-package company
  :ensure nil
  :bind (:map
         company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         :map
         company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)))

(use-package treemacs
  :ensure nil
  :bind (:map global-map
              ("C-t"       . treemacs)))

;; `Additional' - add some new package

;; (use-package vterm ; requires libvterm be installed on system
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'language-specific' overrides
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'C', 'C++' and 'Makefile' specific overrides (an example)

(use-package c-ts-mode
  :ensure nil
  :preface
  (defun grok-c-ts-style ()
    ;; Use Linux kernel coding style in C (Tree-sitter)
    ;; https://www.kernel.org/doc/html/v4.10/process/coding-style.html
    (setq-local indent-tabs-mode t)       ; Use tabs
    (setq-local tab-width 8)              ; Display width of tab
    (setq-local c-ts-mode-indent-style 'linux)
    (setq-local c-ts-mode-indent-offset 8))
  :hook (c-ts-mode . grok-c-ts-style))

(use-package c++-ts-mode
  :ensure nil
  :preface
  (defun grok-cpp-ts-style ()
    ;; Linux kernel-like tabs/8 for C++ (Tree-sitter)
    (setq-local indent-tabs-mode t)
    (setq-local tab-width 8)
    (setq-local c++-ts-mode-indent-style 'linux)
    (setq-local c++-ts-mode-indent-offset 8))
  :hook (c++-ts-mode . grok-cpp-ts-style))

(use-package makefile-mode
  :ensure nil
  :preface
  ;; Tabs are literal in Makefiles
  (defun grok-makefile-tabs ()
    (setq-local indent-tabs-mode t))
  :hook (makefile-mode . grok-makefile-tabs))

(provide 'grok)

;;; grok.el ends here
