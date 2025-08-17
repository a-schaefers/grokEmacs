;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'server)
(or (server-running-p)
    (server-start))

(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))
(setenv "PAGER" "cat")

;; 'PATH' modifications

(setq grokemacs-path-insert '("~/bin"
                            "~/.local/bin"))

(setq grokemacs-path-append '(""))

;; SET matching exec-path and 'PATH' values with inserts/appends

(dolist (item grokemacs-path-insert)
  (add-to-list 'exec-path item))

(dolist (item grokemacs-path-append)
  (add-to-list 'exec-path item t))

(setenv "PATH" (string-trim-right (string-join exec-path ":") ":$"))

(provide 'grok-environment)
