;;; -*- lexical-binding: t; no-byte-compile: t; -*-

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

;; FIX; apparently elpaca will prefer built-in and not pull in what magit needs
;; This can probably go away in the future versions of Emacs
;; https://github.com/progfolio/elpaca/issues/272#issuecomment-2298727726
(setq elpaca-ignored-dependencies
      (delq 'transient elpaca-ignored-dependencies))

(setq use-package-always-defer t)

(use-package emacs
  :ensure nil
  :preface
  ;; ----- defs

  ;; 1) Projects dir
  (defvar grok-projects "~/repos"
    "Absolute path to your projects directory (used by Magit/Projectile/etc).")

  ;; 2) Evil toggle
  (defvar grok-evil-mode nil
    "Non-nil enables Evil-mode setup during bootstrap.")

  ;; 3) Theme gating: t -> configure theme; nil -> angry-fruit vanilla (skip rest).
  (defvar grok-theme t
    "When non-nil, configure a theme; when nil, skip theme/appearance prompts.")

  ;; 4) Theme style: \"fancy\" or \"minimal\"
  (defvar grok-theme-style "fancy"
    "Theme style preference: \"fancy\" or \"minimal\".")

  ;; Fancy-only variables (package theme)
  (defvar grok-custom-theme-pkg 'doom-themes
    "Fancy style: theme package to install/use (e.g., 'doom-themes).")
  (defvar grok-custom-theme 'doom-one
    "Fancy style: theme symbol from the package (e.g., 'doom-one).")

  ;; Minimal-only variable (built-in theme)
  (defvar grok-custom-built-in-theme 'modus-operandi
    "Minimal style: built-in theme symbol (e.g., 'modus-operandi).")

  ;; 5) Alpha background (string: \"0\"–\"99\"; \"\" or \"100\" disables).
  (defvar grok-alpha-background "100"
    "Alpha transparency percent as string \"0\"–\"99\"; \"\" or \"100\" disables.")

  ;; 6) Font family
  (defvar grok-font "Source Code Pro"
    "Preferred monospace font family installed on the system.")

  ;; 7) Font size (string)
  (defvar grok-font-size "11"
    "Preferred font size in points as a string (e.g., \"11\").")

  ;; 8/9) Line numbers (+ optional relative)
  (defvar grok-line-numbers nil
    "Non-nil shows line numbers globally where appropriate.")
  (defvar grok-relative-line-numbers nil
    "Non-nil enables relative line numbers when line numbers are on.")

  ;; ----- interactive initial setup (writes ~/.emacs.d/grok-opts.el)
  (defvar grok-opts-file (expand-file-name "grok-opts.el" user-emacs-directory)
    "Path to the persisted Grok options file.")

  (defun grok--ensure-opts (&optional force)
    "Ensure `grok-opts-file` exists; with FORCE recreate via minibuffer prompts; then load it.
Order:
1) grok-projects
2) grok-evil-mode
3) grok-theme (angry-fruit if nil -> skip remaining)
4) grok-theme-style (\"fancy\"|\"minimal\")
   - minimal -> set grok-custom-theme-pkg=nil; ask built-in theme -> grok-custom-built-in-theme
   - fancy   -> ask package -> grok-custom-theme-pkg; ask theme -> grok-custom-theme
5) grok-alpha-background
6) grok-font
7) grok-font-size
8) grok-line-numbers (+ 9) grok-relative-line-numbers followup"
    (interactive "P")
    (let ((use-dialog-box nil)
          (use-file-dialog nil)
          (opts-file grok-opts-file))
      (when (or force (not (file-exists-p opts-file)))
        (with-temp-file opts-file
          (insert ";;; -*- lexical-binding: t; no-byte-compile: t; -*-\n\n")

          ;; 1) Projects directory
          (let* ((fallback (and (boundp 'grok-projects) grok-projects))
                 (dir (read-directory-name "📂 Projects dir: "
                                           (and fallback (expand-file-name fallback))
                                           nil t)))
            (prin1 `(setq grok-projects ,dir) (current-buffer)) (insert "\n"))

          ;; 2) Evil
          (let ((ans (y-or-n-p "😈 Enable Evil mode? [y] 😇 or Holy mode [n] ")))
            (prin1 `(setq grok-evil-mode ,ans) (current-buffer)) (insert "\n"))

          ;; 3) Theme gating
          (let ((use-theme (y-or-n-p "🧙 Continue to theme customization wizard? 🐊✨ ")))
            (prin1 `(setq grok-theme ,use-theme) (current-buffer)) (insert "\n")

            (when use-theme
              ;; 4) Theme style
              (let* ((style (completing-read "🎨 Theme style (✨ fancy | 📦 minimal): "
                                             '("fancy" "minimal") nil t
                                             (or (and (boundp 'grok-theme-style) grok-theme-style)
                                                 "fancy"))))
                (prin1 `(setq grok-theme-style ,style) (current-buffer)) (insert "\n")

                (if (string= style "minimal")
                    (progn
                      (prin1 `(setq grok-custom-theme-pkg nil) (current-buffer)) (insert "\n")
                      (let* ((def (if (and (boundp 'grok-custom-built-in-theme)
                                           (symbolp grok-custom-built-in-theme))
                                      (symbol-name grok-custom-built-in-theme)
                                    "modus-operandi"))
                             (th (intern (completing-read "🎨 Built-in theme: "
                                                          '("modus-operandi" "modus-vivendi")
                                                          nil nil def))))
                        (prin1 `(setq grok-custom-built-in-theme ',th) (current-buffer)) (insert "\n")))
                  ;; fancy
                  (let* ((defpkg (if (and (boundp 'grok-custom-theme-pkg)
                                          (symbolp grok-custom-theme-pkg))
                                     (symbol-name grok-custom-theme-pkg)
                                   "doom-themes"))
                         (pkg (intern (read-from-minibuffer
                                       "📦 Theme package to install: " defpkg)))
                         (deftheme (if (and (boundp 'grok-custom-theme)
                                            (symbolp grok-custom-theme))
                                       (symbol-name grok-custom-theme)
                                     "doom-one"))
                         (th (intern (read-from-minibuffer
                                      "🎨 Theme name from that package: " deftheme))))
                    (prin1 `(setq grok-custom-theme-pkg ',pkg) (current-buffer)) (insert "\n")
                    (prin1 `(setq grok-custom-theme ',th) (current-buffer)) (insert "\n"))))

              ;; 5) Alpha background
              (let* ((def (or (and (boundp 'grok-alpha-background) grok-alpha-background) ""))
                     (val (read-string "🪟 Transparency (0–99, [enter]/100 = off): " def)))
                (prin1 `(setq grok-alpha-background ,val) (current-buffer)) (insert "\n"))

              ;; 6) Font family
              (let* ((def (or (and (boundp 'grok-font) grok-font) "Source Code Pro"))
                     (val (read-string "🔤 Font (must be on system): " def)))
                (prin1 `(setq grok-font ,val) (current-buffer)) (insert "\n"))

              ;; 7) Font size
              (let* ((def (or (and (boundp 'grok-font-size) grok-font-size) "11"))
                     (val (read-string "📏 Font Size: " def)))
                (prin1 `(setq grok-font-size ,val) (current-buffer)) (insert "\n"))

              ;; 8) Line numbers (+ 9) relative follow-up
              (let ((ln (y-or-n-p "🔢 Show line numbers? ")))
                (prin1 `(setq grok-line-numbers ,ln) (current-buffer)) (insert "\n")
                (when ln
                  (let ((rel (y-or-n-p "🔢➡️ Use relative line numbers? ")))
                    (prin1 `(setq grok-relative-line-numbers ,rel) (current-buffer)) (insert "\n"))))))))

      (load opts-file t t)))

  :init
  ;; ----- first things
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
