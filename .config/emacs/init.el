;; Package Manager
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Keybindings
;;; General
(use-package general
  :ensure t
  :config
  (general-define-key
   "C-x C-b" 'persp-ibuffer
   "C-x b" 'persp-switch-to-buffer
   "C-c f" 'recentf
   "M-o" 'other-window
   "C-x r b" 'consult-bookmark
   "C-c p s" 'persp-switch))

;; General Emacs Settings
;;; UI
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(column-number-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Font Setting
(set-face-attribute 'default nil :font "Mononoki Nerd Font Mono" :height 160)
(setq-default line-spacing 5)

;;; Editing
(global-visual-line-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(setq-default indent-tabs-mode nil)

;;; Autosaves and Backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;;; Emacs
(use-package emacs
  :init
  (savehist-mode 1)
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

;; UI Improvements
;;; Color Scheme

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-duo-dark t))

;;; Icons
;;; This is to properly render nerd icons when using doom-modeline.
(use-package nerd-icons
  :ensure t)

;;; Mode-line
(setq doom-modeline-support-imenu t)
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-lsp-icon t))

;;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

;;; Dashboard setup
(setq dashboard-banner-logo-title "Heads or tails?")
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-vertically-center-content t)
(setq dashboard-projects-backend 'projectile)

(setq dashboard-items '((projects . 10)
                        (recents . 5)
                        (bookmarks . 5)
                        (agenda . 5)))

(setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-navigator
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items
                                  dashboard-insert-newline
                                  dashboard-insert-footer))

(setq dashboard-display-icons-p t)
(setq dashboard-icon-type 'nerd-icons)

(setq dashboard-projects-switch-function 'projectile-persp-switch-project)

;;; Dashboard dependencies
(use-package page-break-lines
  :ensure t
  :config
  (page-break-lines-mode))

;;; Diminish
;;; Used to not show minor modes when using some packages.
(use-package diminish
  :ensure t)

;;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Org Mode
;;; Configure Babel Languages
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (C . t)
     (java . t)
     (js . t))))

;;; Table of Contents
(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init
  (add-hook 'org-mode-hook 'toc-org-enable))

;;; Org Tempo
;;; For quickly creating source code blocks in Org Mode.
(require 'org-tempo)

;;; Org Bullets 
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; Org Roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/cvdcg/vinci-roam-notes/")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

;;; org roam dependency
(use-package emacsql-sqlite
  :ensure t)

;;; Org Roam UI
;;; Package that shows a graph for org roam notes
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;;; Org Roam UI Dependency
(use-package websocket
  :ensure t
  :after org-roam)

;;; Additional org mode configuration
(setq org-image-actual-width nil)

;; Completion
;;; Vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq enable-recursive-minibuffers t)
  (setq set-vertico-count 20)
  (setq vertico-cycle t))

;;; Marginalia
;;; For annotations in the minibuffer.
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

;;; Orderless
;;; Backend completion style.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Consult
;;; Backend completion functions.
(use-package consult
  :ensure t)

;;; Embark
;;; Actions on completion buffer.
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Corfu
;;; In buffer frontend completion UI.
(use-package corfu
  :ensure t
  :hook
  ((prog-mode . corfu-mode)
   (shell-mode . corfu-mode)
   (eshell-mode . corfu-mode))
  :custom
  (corfu-auto t))

;;; Cape
;;; In buffer backend completion functions.
(use-package cape
  :ensure t
  :bind
  ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Project Management
;;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(use-package ripgrep
  :ensure t)

;;; Perspective
(use-package perspective
  :ensure t
  :init
  (persp-mode)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p")))

(use-package persp-projectile
  :ensure t)

;; Helper packages
;;; Which Key
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; DEVELOPMENT

;;; Treesitter
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Git
(use-package magit
  :ensure t)

;;; transient package is used to fix the dependency issues of magit
(use-package transient
  :ensure t)

;;; EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; Snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

;;; Terminal Emulator
(use-package vterm
  :ensure t)

;;; Markdown Mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))

;; PATH Configuration
;;; exec-path-from-shell is a package that copies the shell PATH variable to Emacs
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("159a29ab0ec5ba4e2811eddd9756aa779b23467723dcbdd223929fbf2dde8954" "355e3439089e3b37bb143afc0a60ce091533fe467db2ab0f2ae34d13be7a47c5" "7776ba149258df15039b1f0aba4b180d95069b2589bc7d6570a833f05fdf7b6d" "ccff17f0cb616e239e2de4bd78f0b2e8f1f49291aa43c50845e250203be27a95" "515ebca406da3e759f073bf2e4c8a88f8e8979ad0fdaba65ebde2edafc3f928c" "263e3a9286c7ab0c4f57f5d537033c8a5943e69d142e747723181ab9b12a5855" "ed1b7b4db911724b2767d4b6ad240f5f238a6c07e98fff8823debcfb2f7d820a" "b216e9b72dc8c2b702e4fcfd3c0af2d73c87eba46fd4db824ddb50863447d6a9" default))
 '(package-selected-packages '(eglot)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
