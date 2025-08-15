;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(add-hook 'elpaca-after-init-hook #'(lambda ()
                                      (interactive)
                                      (require 'server)
                                      (or (server-running-p)
                                          (server-start))))

(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))
(setenv "PAGER" "cat")
