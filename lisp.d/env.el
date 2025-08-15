;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'server)
(or (server-running-p)
    (server-start))

(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))
(setenv "PAGER" "cat")
