;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;----- global theme settings

(defun grok--font-size->height (&optional s)
  (setq grok-font-size-translated (round (* 10 (string-to-number (or s grok-font-size))))))

(if (find-font (font-spec :name grok-font))
    (set-face-attribute 'default nil :family grok-font :height (grok--font-size->height grok-font-size))
  ;;fallback nicely to Monospace system font
  (progn
    (set-face-attribute 'default nil :family "Monospace" :height (grok--font-size->height grok-font-size))
    (message (concat grok-font " font unavailable, falling back to Monospace"))))

(blink-cursor-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq-default frame-title-format '("GNU Emacs"))

(when (and (boundp 'grok-alpha-background)
           (stringp grok-alpha-background)
           (not (or
                 (string= grok-alpha-background "")
                 (string= grok-alpha-background "100"))))
  (set-frame-parameter nil 'alpha-background
                       (string-to-number grok-alpha-background)))

(add-hook 'prog-mode-hook
          (lambda ()
            (when (and (boundp 'grok-line-numbers) grok-line-numbers)
              (setq-local display-line-numbers
                          (if (and (boundp 'grok-relative-line-numbers) grok-relative-line-numbers)
                              'relative t))
              (display-line-numbers-mode 1))))


 (provide 'grok-theme)
