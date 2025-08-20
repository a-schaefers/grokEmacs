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
  ;; 'env' modifications
  (defvar grok-env
  '((EDITOR . "emacsclient")
    (VISUAL . "$EDITOR")
    (PAGER  . "cat"))
  "Alist of environment variables to set when `grok-apply-env-and-path' runs.")

  ;; 'PATH' modifications
  (defvar grok-path-insert '(
                             ;; e.g.:
                             ;; "~/bin"
                             ;; "~/.local/bin"
                             )
    "Extra PATH entries to prepend to `exec-path' and $PATH.")

  (defvar grok-path-append '("")
    "Extra PATH entries to append to `exec-path' and $PATH.")

  (defun grok-apply-env-and-path ()
    "Apply ENV/PATH customization."
    (require 'subr-x)

    ;; ENV
    (dolist (pair grok-env)
      (let* ((var (if (symbolp (car pair)) (symbol-name (car pair)) (car pair)))
             (raw (cdr pair))
             (val (substitute-env-vars (if (stringp raw) raw (format "%s" raw)))))
        (setenv var val)))

    ;; PATH
    (dolist (item grok-path-insert)
      (add-to-list 'exec-path (expand-file-name item)))
    (dolist (item grok-path-append)
      (add-to-list 'exec-path (expand-file-name item) t))
    (setenv "PATH" (string-trim-right (string-join exec-path ":") ":$")))
  :init
  (grok-apply-env-and-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'LSP' - Modes that will autostart the corresponding server if found on PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil
  :preface
  ;; Eglot autostart map: major-mode hook -> server command.
  (defvar grok-eglot-autostart-langs
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
      (csharp-ts-mode-hook     . "OmniSharp"))
    "Alist of major-mode hooks -> language servers for Eglot autostart.")

  (defun grok-apply-eglot-autostart (&optional table)
  "Register Eglot autostart hooks from TABLE (alist HOOK . SPEC)."
  (require 'subr-x)
  (dolist (pair (or table grok-eglot-autostart-langs))
    (let* ((hook (car pair))
           (spec (cdr pair))
           (mode (intern (string-remove-suffix "-hook" (symbol-name hook))))
           (override nil)
           (cmd nil))
      (cond
       ((stringp spec)
        (setq cmd (list spec)))
       ((consp spec)
        (when (eq (car spec) :override)
          (setq override t
                spec (cdr spec)))
        (cond
         ((stringp spec)
          (setq cmd (list spec)))
         ((and (listp spec)
               (let ((all-strings t))
                 (dolist (s spec)
                   (unless (stringp s) (setq all-strings nil)))
                 all-strings))
          (setq cmd spec)))))
      (when (and override cmd)
        (with-eval-after-load 'eglot
          (add-to-list 'eglot-server-programs (cons mode cmd))))
      (when (and cmd (executable-find (car cmd)))
        (add-hook hook #'eglot-ensure)))))
  :init
  (grok-apply-eglot-autostart))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'General'
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil
  :preface
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

  (defun grok-pop-flymake-diagnostics ()
    (interactive)
    (flymake-mode 1)
    (flymake-show-diagnostics-buffer))

  :init
  ;; Python friendly
  (setq-default indent-tabs-mode nil
                fill-column 79)

  ;; Set default compile command, for M-x cc
  (setq compile-command "make -k ")

  ;; Some global binds
  :bind (("C-c gi" . grok-edit-init-file)
         ("C-c gg" . grok-edit-grok-file)
         ("C-c go" . grok-edit-grok-initial-setup-opts) ; or just C-u M-x grok--ensure-opts
         ("C-c gu" . grok-update-config-with-ediff))

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

;; 'Overrides' - hack on some already provided package by core

(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("M-m r" . eglot-rename)
              ("M-m o" . eglot-code-action-organize-imports)
              ("M-m h" . eldoc)
              ("M-m =" . eglot-format)
              ("M-m ?" . xref-find-references)
              ("M-."   . xref-find-definitions)))

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

;; `Additional' - add some new package

;; requires libvterm be installed on system
;; (use-package vterm
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
