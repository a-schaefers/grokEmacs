;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package emacs
  :ensure nil
  :preface
  ;;-----defs
  (defvar grok-projects "~/repos"
    "Absolute path to your projects directory (used by Magit/Projectile/etc).")

  (defvar grok-evil-mode nil
    "Non-nil enables Evil-mode setup during bootstrap.")

  (defvar grok-theme-mode "dark"
    "Theme brightness preference: \"light\" or \"dark\".")

  (defvar grok-theme-style "fancy"
    "Theme style preference: \"fancy\" or \"minimal\".")

  (defvar grok-font "Source Code Pro"
    "Preferred monospace font family installed on the system.")

  (defvar grok-font-size "11"
    "Preferred font size in points as a string (e.g., \"12\").")

  (defvar grok-alpha-background "100"
    "Alpha transparency percent as string \"0\"–\"99\"; \"\" or \"100\" disables.")

  ;;-----interactive initial setup

  (defvar grok-opts-file (expand-file-name "grok-opts.el" user-emacs-directory)
    "Path to the persisted Grok options file.")

  (defvar grok-opts-spec
    '((grok-projects        :type directory :prompt "Projects dir: " :default "~/repos")
      (grok-evil-mode            :type boolean   :prompt "Enable evil mode? ")
      (grok-theme           :type boolean   :prompt "Use a preconfigured theme? (No will provide a pure-vanilla angry-fruit Emacs) ")
      (grok-theme-style     :type choice    :prompt "Theme (fancy/minimal/none): " :choices (fancy minimal none) :default fancy)
      (grok-theme-mode      :type choice    :prompt "Theme (light/dark): " :choices (light dark) :default dark)
      (grok-alpha-background :type string   :prompt "Transparency (0–99, [enter]/100 = off): " :default "")
      (grok-font            :type string    :prompt "Font (must be on system): " :default "Source Code Pro")
      (grok-font-size       :type string    :prompt "Font Size: " :default "11")
      (grok-line-numbers    :type boolean   :prompt "Show line numbers? "))
    "Declarative prompts for Grok options. Extend by appending new entries.")

  (defun grok--ensure-opts (&optional force)
    "Ensure `grok-opts-file` exists; with FORCE (C-u) recreate using minibuffer prompts; then load it."
    (interactive "P")
    (let* ((use-dialog-box nil)    ;; force minibuffer prompts
           (use-file-dialog nil)
           (opts-file grok-opts-file)
           ;; gating: if user says NO to grok-theme, all subsequent answers become nil
           (seen-theme nil)
           (skip-after-theme nil))
      (when (or force (not (file-exists-p opts-file)))
        (with-temp-file opts-file
          (insert ";;; -*- lexical-binding: t; no-byte-compile: t; -*-\n\n")
          (dolist (spec grok-opts-spec)
            (let* ((sym      (car spec))
                   (plist    (cdr spec))
                   (type     (plist-get plist :type))
                   (prompt   (or (plist-get plist :prompt) (format "%s: " sym)))
                   (choices  (plist-get plist :choices))
                   (fallback (or (and (boundp sym) (symbol-value sym))
                                 (plist-get plist :default)))
                   val)
              (setq val
                    (if (and skip-after-theme seen-theme (not (eq sym 'grok-theme)))
                        ;; After grok-theme=nil, force all remaining values to nil without prompting
                        nil
                      (pcase type
                        ('directory (read-directory-name prompt
                                                         (and fallback (expand-file-name fallback))
                                                         nil nil))
                        ('boolean  (y-or-n-p prompt))
                        ('choice   (let* ((def (cond ((stringp fallback) fallback)
                                                     ((symbolp fallback) (symbol-name fallback))
                                                     (t nil))))
                                     (completing-read prompt
                                                      (mapcar #'symbol-name choices)
                                                      nil t def)))
                        ('string   (read-string prompt (or fallback "")))
                        (_         (read-from-minibuffer prompt (format "%s" (or fallback "")))))))
              (prin1 `(setq ,sym ,val) (current-buffer))
              (insert "\n")
              ;; Flip the gating switch immediately after answering grok-theme
              (when (eq sym 'grok-theme)
                (setq seen-theme t
                      skip-after-theme (not val)))
              ;; Follow-up: if line numbers are enabled, ask for relative numbers.
              (when (and (eq sym 'grok-line-numbers) val)
                (let ((rel (y-or-n-p "Use relative line numbers? ")))
                  (prin1 `(setq grok-relative-line-numbers ,rel) (current-buffer))
                  (insert "\n")))))))
      (load opts-file t t)))

  :init
  ;;-----first things
  (setq load-prefer-newer t
        gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)
        comp-deferred-compilation t
        package-native-compile t)

  ;; don't pop-up compilation warnings during native compiles / confuse the user
  (setq native-comp-async-report-warnings-errors nil
        comp-async-report-warnings-errors nil)
  (add-to-list 'warning-suppress-types '(native-compiler))
  (add-to-list 'warning-suppress-types '(native-compiler))
  (add-to-list 'display-buffer-alist '("\\*Warnings\\*" (display-buffer-no-window) (allow-no-window . t)))

  :config
  (grok--ensure-opts))

(provide 'grok-bootstrap)
