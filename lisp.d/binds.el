;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(global-set-key (kbd "C-c i") #'crux-find-user-init-file)

;; M-x cc, sh, lint, git, pro
(defalias 'cc 'compile)
(defalias 'sh 'shell)
(defun lint ()
    (interactive)
    (flymake-mode 1)
    (flymake-show-diagnostics-buffer))
(defalias 'git 'magit)
(defalias 'pro 'projectile-commander)

(defun force-kill-this-window ()
  "Forcefully kill the buffer in the current window, terminating any running process, and close the window."
  (interactive)
  (let* ((buf (current-buffer))
         (proc (get-buffer-process buf)))
    (when proc
      (ignore-errors
        (set-process-query-on-exit-flag proc nil)
        (kill-process proc)))
    (kill-buffer buf)
    (delete-window)))
(global-set-key (kbd "s-<backspace>") #'force-kill-this-window)

(global-set-key (kbd "C-1") #'delete-other-windows)   ;; C-x 1
(global-set-key (kbd "C-2") #'split-window-below)     ;; C-x 2
(global-set-key (kbd "C-3") #'split-window-right)     ;; C-x 3
(global-set-key (kbd "C-0") #'delete-window)          ;; C-x 0

(with-eval-after-load 'crux
  (global-set-key (kbd "C-<tab>") #'crux-other-window-or-switch-buffer))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "M-m r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-m o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "M-m h") 'eldoc)
  (define-key eglot-mode-map (kbd "M-m =") 'eglot-format)
  (define-key eglot-mode-map (kbd "M-m ?") 'xref-find-references)
  (define-key eglot-mode-map (kbd "M-.")   'xref-find-definitions))

;; Auto-completion bindings
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous))
