;;; init.el --- Initialization file for Emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Prevent package.el from interfering with Elpaca by disabling it at startup.
;; I've observed in some cases it's not enough to do this in regular init.el,
;; so here we apply it as early as possible for most users.

;;; Code:

(setq package-enable-at-startup nil)

;;; early-init.el ends here
