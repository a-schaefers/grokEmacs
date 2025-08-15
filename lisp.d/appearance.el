;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(setq inhibit-startup-screen nil)

(load-theme 'modus-operandi t)

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
  aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(progn
  (setq-default mode-line-format
                '((:eval (simple-mode-line-render
                          ;; left
                          (format-mode-line "%* %b %l")
                          ;; right
                          (format-mode-line "%m"))))))

(set-face-attribute 'default nil :family "Monospace" :height 120)

(blink-cursor-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default frame-title-format '("GNU Emacs"))

;; Set transparency
;; (set-frame-parameter nil 'alpha-background '90)
