;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;----- minimal theme settings

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
  aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun grok-minimal-modeline ()
  (progn
    (setq-default mode-line-format
                  '((:eval (simple-mode-line-render
                            ;; left
                            (format-mode-line "%* %b %l")
                            ;; right
                            (format-mode-line "%m")))))))


(and (string= grok-theme-mode "dark") (string= grok-theme-fancy "minimal")
    (progn
      (setq inhibit-startup-screen nil)
      (load-theme 'modus-vivendi t)
      (grok-minimal-modeline)))

(and (string= grok-theme-mode "light") (string= grok-theme-fancy "minimal")
    (progn
      (setq inhibit-startup-screen nil)
      (load-theme 'modus-operandi t)
      (grok-minimal-modeline)))


 (provide 'grok-theme-minimal)
