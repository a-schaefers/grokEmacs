;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(setq load-prefer-newer t
      gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      comp-deferred-compilation t
      package-native-compile t)

(setq native-comp-async-report-warnings-errors nil
      comp-async-report-warnings-errors nil)
(add-to-list 'warning-suppress-types '(native-compiler))
(add-to-list 'warning-suppress-types '(native-compiler))
(add-to-list 'display-buffer-alist '("\\*Warnings\\*" (display-buffer-no-window) (allow-no-window . t)))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setq lisp-d (concat user-emacs-directory "lisp.d"))
(defun lisp-user-local-hook ()
  (when (file-directory-p lisp-d)
    (dolist (file (directory-files lisp-d nil "^.*\.el$"))
      (load-file (concat lisp-d "/" file)))))
(add-hook 'elpaca-after-init-hook 'lisp-user-local-hook)

(use-package vertico
  :ensure t
  :demand t
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (vertico-mode 1)
  (savehist-mode 1)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

(use-package prescient :ensure t :demand t)

(use-package vertico-prescient
  :ensure t
  :demand t
  :after (vertico prescient)
  :config
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package company-prescient
  :after (company prescient)
  :ensure t
  :demand t
  :config (company-prescient-mode 1))

(use-package company
  :ensure t
  :demand t
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'html-ts-mode-hook 'company-mode) ; apparently not a prog-mode

  (setq company-idle-delay 0))

(use-package transient :ensure t :demand t)
(use-package magit
  :ensure t
  :demand t
  :after (transient))

(use-package projectile
  :ensure t
  :demand t
  :config
  (projectile-mode 1)

  (with-eval-after-load 'magit
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    (projectile-save-known-projects)))

(use-package yasnippet
  :ensure t
  :demand t
  :hook (prog-mode-hook . yas-minor-mode))

(use-package eglot
  :after yasnippet
  :ensure t
  :demand t)

(use-package flymake
  :ensure t
  :demand t
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package crux
 :ensure t
 :demand t)

(use-package better-shell
  :ensure t
  :demand t
  :config
  (setq explicit-bash-args '("--noediting" "-i" "-l"))

  (add-hook 'shell-mode-hook #'(lambda ()
                                 (interactive)
                                 (setq comint-input-ring-file-name "~/.bash_history")
                                 (comint-read-input-ring t)
                                 (comint-write-input-ring))))

(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package paredit
   :ensure t
   :demand t
   :config
   (add-hook 'emacs-lisp-mode-hook        #'enable-paredit-mode)
   (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
   (add-hook 'ielm-mode-hook               #'enable-paredit-mode)
   ;; lisps
   (add-hook 'lisp-interaction-mode-hook   #'enable-paredit-mode)
   (add-hook 'lisp-mode-hook               #'enable-paredit-mode)
   ;; schemes
   (add-hook 'scheme-mode-hook             #'enable-paredit-mode)
   ;; clojure
   (with-eval-after-load 'clojure-mode
     (add-hook 'clojure-mode-hook          #'enable-paredit-mode))
   ;; racket
   (with-eval-after-load 'racket-mode
     (add-hook 'racket-mode-hook           #'enable-paredit-mode)))

;; (use-package evil
;;   :ensure t
;;   :demand t
;;   :config (evil-mode 1))

;; (use-package evil-commentary
;;   :ensure t
;;   :demand t
;;   :after (evil)
;;   :config (evil-commentary-mode 1))
