;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; better scratch https://www.reddit.com/r/emacs/comments/4cmfwp/scratch_buffer_hacks_to_increase_its_utility/

(setq initial-major-mode 'org-mode)

(setq initial-scratch-message "* Spartan Emacs
Finally, a simple base Emacs configuration framework

** Where to start
- Press ~C-c i~ to jump to ~/.emacs.d/spartan.el and edit your config

** How to keep up to date
Layers use pinned packages via Straight.el and are periodically updated.

0. cd ~/.emacs.d
1. git pull
2. M-x straight-thaw-versions
3. Restart Emacs

** About this *scratch* buffer
This org-mode buffer is  un-killable and persistent in ~/.emacs.d/scratch
Use it for whatever you want, or don't.")

(defun immortal-scratch ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil)
  t))

(defun save-persistent-scratch ()
  "Save the contents of *scratch*"
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max)
                  (concat user-emacs-directory "scratch"))))

(defun load-persistent-scratch ()
  "Reload the scratch buffer"
  (let ((scratch-file (concat user-emacs-directory "scratch")))
    (if (file-exists-p scratch-file)
        (with-current-buffer (get-buffer "*scratch*")
          (delete-region (point-min) (point-max))
          (insert-file-contents scratch-file)))))

(progn
  (add-hook 'kill-buffer-query-functions 'immortal-scratch)
  (add-hook 'after-init-hook 'load-persistent-scratch)
  (add-hook 'kill-emacs-hook 'save-persistent-scratch)
  (run-with-idle-timer 300 t 'save-persistent-scratch))

(provide 'spartan-better-scratch)
