;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; C and C++ specific overrides (A language-specific override example)

(defun grokemacs-c-ts-modes ()
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

(add-hook 'c-ts-mode-hook #'grokemacs-c-ts-modes)
(add-hook 'c++-ts-mode-hook #'grokemacs-c-ts-modes)

;; tabs are tabs in C family langs
(add-hook 'makefile-mode-hook (lambda ()
                                (setq-local indent-tabs-mode t)))

(provide 'grok-lang-c-cpp)
