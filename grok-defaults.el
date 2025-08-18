;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; Local customization file (not under version control).
;; Loads last

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'ENV' / 'PATH' customization
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'env' modifications

(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))
(setenv "PAGER" "cat")

;; 'PATH' modifications

(setq grok-path-insert '("~/bin"
                         "~/.local/bin"))

(setq grok-path-append '(""))

;; SET matching exec-path and 'PATH' values with inserts/appends

(dolist (item grok-path-insert)
  (add-to-list 'exec-path item))

(dolist (item grok-path-append)
  (add-to-list 'exec-path item t))

(setenv "PATH" (string-trim-right (string-join exec-path ":") ":$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'LSP' - Modes that will autostart the corresponding server if found on PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq my-eglot-autostart-langs
      '(
        (c-ts-mode-hook . clangd)
        (c++-ts-mode-hook . clangd)
        (lua-ts-mode-hook . lua-language-server)
        (bash-ts-mode-hook . bash-language-server)
        (python-ts-mode-hook . pylsp)
        (go-ts-mode-hook . gopls)
        (rust-ts-mode-hook . rust-analyzer)
        (ruby-ts-mode-hook . solargraph)
        (elixir-ts-mode-hook . (:override elixir-ls))
        (html-ts-mode-hook . vscode-html-language-server)
        (css-ts-mode-hook . vscode-css-language-server)
        (typescript-ts-mode-hook . typescript-language-server)
        (js-ts-mode-hook . typescript-language-server)
        (yaml-ts-mode-hook . yaml-language-server)
        (json-ts-mode-hook . vscode-json-languageserver)
        (java-ts-mode-hook . jdtls)
        (csharp-ts-mode-hook . OmniSharp)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `General'
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start Emacs server

(require 'server)
(or (server-running-p)
    (server-start))

;; Set default compile command, for make or whatever.
(setq compile-command "make -k ")

(setq-default
 indent-tabs-mode nil ; In general, prefer spaces
 fill-column 79)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'C' and 'C++' specific overrides (A language-specific override example)
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grok-c-ts-modes ()
  ;; Use Linux kernel coding style in C and C++ (Tree-sitter modes)
  ;; https://www.kernel.org/doc/html/v4.10/process/coding-style.html
  (setq-local indent-tabs-mode t)               ; Use tabs
  (setq-local tab-width 8)                      ; Display width of tab
  ;; C-specific
  (setq-local c-ts-mode-indent-style 'linux)
  (setq-local c-ts-mode-indent-offset 8)
  ;; C++-specific
  (setq-local c++-ts-mode-indent-style 'linux)
  (setq-local c++-ts-mode-indent-offset 8))

(add-hook 'c-ts-mode-hook #'grok-c-ts-modes)
(add-hook 'c++-ts-mode-hook #'grok-c-ts-modes)

;; tabs are tabs in C family langs
(add-hook 'makefile-mode-hook (lambda ()
                                (setq-local indent-tabs-mode t)))

(provide 'grok)
