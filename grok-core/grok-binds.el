;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (find-file (expand-file-name "init.el" user-emacs-directory))))

;; M-x cc, sh, lint, git, pro

(defalias 'cc 'compile)

(defun lint ()
    (interactive)
    (flymake-mode 1)
    (flymake-show-diagnostics-buffer))

(defalias 'git 'magit)

(defalias 'pro 'projectile-commander)

(defalias 'sh 'shell)

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "M-m r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-m o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "M-m h") 'eldoc)
  (define-key eglot-mode-map (kbd "M-m =") 'eglot-format)
  (define-key eglot-mode-map (kbd "M-m ?") 'xref-find-references)
  (define-key eglot-mode-map (kbd "M-.")   'xref-find-definitions))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous))

(with-eval-after-load 'crux
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-o") 'crux-smart-open-line)
  (global-set-key (kbd "C-x C-o") 'crux-other-window-or-switch-buffer)
  (global-set-key (kbd "C-c C-l") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c C--") 'crux-kill-whole-line)
  (global-set-key (kbd "C-c ;") 'crux-duplicate-and-comment-current-line-or-region))

(provide 'grok-binds)
