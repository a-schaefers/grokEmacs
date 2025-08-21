;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package emacs
  :ensure nil
  :preface
  (defun grok/immortal-scratch ()
    (if (eq (current-buffer) (get-buffer "*scratch*"))
        (progn (bury-buffer) nil) t))
  (defun grok/save-persistent-scratch ()
    "Save the contents of *scratch* to ~/.config/emacs/scratch."
    (with-current-buffer (get-buffer-create "*scratch*")
      (write-region (point-min) (point-max)
                    (expand-file-name "scratch" user-emacs-directory))))
  (defun grok/load-persistent-scratch ()
    "Reload the *scratch* buffer from ~/.config/emacs/scratch."
    (let ((f (expand-file-name "scratch" user-emacs-directory)))
      (when (file-exists-p f)
        (with-current-buffer (get-buffer "*scratch*")
          (erase-buffer)
          (insert-file-contents f)))))
  :init
  (setq initial-major-mode 'org-mode
        initial-scratch-message
        "* About this *scratch* buffer
This org-mode buffer is un-killable and persistent")
  :hook ((kill-buffer-query-functions . grok/immortal-scratch)
         (elpaca-after-init . grok/load-persistent-scratch)
         (kill-emacs . grok/save-persistent-scratch))
  :config
  (run-with-idle-timer 300 t #'grok/save-persistent-scratch))

(provide 'grok-better-scratch)
