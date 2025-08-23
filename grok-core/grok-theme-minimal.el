;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;----- minimal theme settings

(use-package emacs
  :ensure nil
  :preface
  (defun simple-mode-line-render (left right)
    "Return a string of `window-width' length containing LEFT, and RIGHT
  aligned respectively."
    (let* ((available-width (- (window-width) (length left) 2)))
      (format (format " %%s %%%ds " available-width) left right)))

  (defun grok-minimal-modeline ()
    (setq-default mode-line-format
                  '((:eval (simple-mode-line-render
                            ;; left
                            (format-mode-line "%* %b %l")
                            ;; right
                            (format-mode-line "%m"))))))
  :config
  (setq inhibit-startup-screen nil)
  (load-theme grok-custom-built-in-theme t)

  (when (string= grok-use-modeline "minimal") ; none skips this and sets default
      (grok-minimal-modeline)))

(provide 'grok-theme-minimal)
