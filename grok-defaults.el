;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; Local customization file (not under version control).
;; Loads last

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'ENV' / 'PATH' customization
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'env' modifications

(use-package emacs
  :ensure nil
  :init
  ;; 'env' modifications
  (setenv "EDITOR" "emacsclient")
  (setenv "VISUAL" (getenv "EDITOR"))
  (setenv "PAGER" "cat")

  ;; 'PATH' modifications
  (setq grok-path-insert '(
                           ;; e.g.:
                           ;;
                           ;; "~/bin"
                           ;; "~/.local/bin"
                           ))
  (setq grok-path-append '(""))

  ;; Set matching 'exec-path' and 'PATH' values with inserts/appends
  (dolist (item grok-path-insert)
    (add-to-list 'exec-path item))
  (dolist (item grok-path-append)
    (add-to-list 'exec-path item t))
  (require 'subr-x)                         ;; string-join / string-trim-right
  (setenv "PATH" (string-trim-right (string-join exec-path ":") ":$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'LSP' - Modes that will autostart the corresponding server if found on PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil
  :preface
  (defvar grok-eglot-autostart-langs
    '(
      (c-ts-mode-hook        . clangd)
      (c++-ts-mode-hook      . clangd)
      (lua-ts-mode-hook      . lua-language-server)
      (bash-ts-mode-hook     . bash-language-server)
      (python-ts-mode-hook   . pylsp)
      (go-ts-mode-hook       . gopls)
      (rust-ts-mode-hook     . rust-analyzer)
      (ruby-ts-mode-hook     . solargraph)
      (elixir-ts-mode-hook   . (:override elixir-ls))
      (html-ts-mode-hook     . vscode-html-language-server)
      (css-ts-mode-hook      . vscode-css-language-server)
      (typescript-ts-mode-hook . typescript-language-server)
      (js-ts-mode-hook       . typescript-language-server)
      (yaml-ts-mode-hook     . yaml-language-server)
      (json-ts-mode-hook     . vscode-json-languageserver)
      (java-ts-mode-hook     . jdtls)
      (csharp-ts-mode-hook   . OmniSharp)

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
      )
    "Alist of major-mode hooks -> language servers for Eglot autostart."))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'Packages' - override core's already provided packages in this way
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'Overrides' - already provided by core

;; (use-package magit
;;   :ensure nil
;;   :config ... )


;; `Additional' - new pkgs

;; (use-package foo
;;   :ensure t
;;   :config ... )

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `General'
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil
  :init
  ;; Python friendly
  (setq-default indent-tabs-mode nil
                fill-column 79)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; `Binds'
  ;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Set default compile command, for M-x cc
  (setq compile-command "make -k ")

  :bind (("C-c gi" . grok-edit-init-file)
         ("C-c gg" . grok-edit-grok-file)
         ("C-c go" . grok-edit-grok-initial-setup-opts) ; or just C-u M-x grok--ensure-opts
         ("C-c gu" . grok-update-config-with-ediff))

  :config
  ;; Start Emacs server
  (require 'server)
  (unless (server-running-p) (server-start))

  ;; M-x cc
  (defalias 'cc 'compile)

  ;; M-x lint
  (defun lint ()
    (interactive)
    (flymake-mode 1)
    (flymake-show-diagnostics-buffer))

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
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'language-specific' overrides
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'C', 'C++' and 'Makefile' specific overrides (A  example)

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
  :after c-ts-mode
  :commands (c++-ts-mode)
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
  :commands (makefile-mode)
  :preface
  (defun grok-makefile-tabs ()            ; Tabs are literal in Makefiles
    (setq-local indent-tabs-mode t))
  :hook (makefile-mode . grok-makefile-tabs))

(provide 'grok)
