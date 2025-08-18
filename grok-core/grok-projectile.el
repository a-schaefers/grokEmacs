;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; (use-package projectile
;;   :ensure t
;;   :after magit

;;   ;; consumes magit & feeds dashboard about our project info, so no deferral
;;   :demand t

;;   :config
;;   (projectile-mode 1)

;;   (with-eval-after-load 'magit
;;     (mapc #'projectile-add-known-project
;;           (mapcar #'file-name-as-directory (magit-list-repos)))
;;     (projectile-save-known-projects)))

(use-package projectile
  :ensure t
  :init
  ;; seed known projects from `magit-repository-directories` without loading Magit
  (dolist (entry magit-repository-directories)
    (projectile-add-known-project (expand-file-name (car entry))))
  (projectile-save-known-projects)
  :config
  (projectile-mode 1))

(provide 'grok-projectile)
