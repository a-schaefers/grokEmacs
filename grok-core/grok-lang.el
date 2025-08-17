;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; Set default compile command, for make or whatever.
(setq compile-command "make -k ")

(setq-default
 indent-tabs-mode nil ; In general, prefer spaces
 fill-column 79
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes that will autostart the corresponding eglot LSP server if found on PATH
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

(defun configure-eglot-autostart ()
  "Configure eglot autostart hooks for specified language modes."
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
                        '(,mode . ,cmd)))))))

(configure-eglot-autostart)

(provide 'grok-lang)
