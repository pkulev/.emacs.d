;; -*- lexical-binding: t -*-
;; This file was tangled (automatically generated) from `readme.org'

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)

(setq package-archives
      (append (eval (car (get 'package-archives 'standard-value)))
              '(("org" . "http://orgmode.org/elpa/")
                ("melpa" . "http://melpa.org/packages/")
                ("elpy" . "https://jorgenschaefer.github.io/packages/"))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(put 'use-package 'lisp-indent-function 1)

(use-package use-package-core
  :custom
  (use-package-always-defer t)
  (use-package-compute-statistics t)
  (use-package-enable-imenu-support t))

(use-package bind-key
  :ensure t
  :demand t)

(use-package delight
  :ensure t
  :demand t)

(use-package gcmh
  :ensure t
  :delight
  :init
  (gcmh-mode 1))

(use-package system-packages
  :ensure t
  :demand t
  :custom
  (system-packages-noconfirm t)
  :config
  ;; Termux has no `sudo'
  (when (string-match-p "termux" (getenv "PATH"))
    (setq system-packages-use-sudo t))
  ;; Overwrite guix even if it installed
  (when (string-match-p "redhat" system-configuration)
    (setq system-packages-package-manager 'dnf)))

(use-package use-package-ensure-system-package
  :ensure t
  :demand t)

(use-package quelpa
  :ensure t
  :demand t
  :custom (quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :ensure t
  :demand t)

(use-package fnhh
  :quelpa
  (fnhh :repo "a13/fnhh" :fetcher github)
  :config
  (fnhh-mode 1))

(use-package paradox
  :ensure t
  :demand t
  :custom
  (paradox-execute-asynchronously t)
  :config
  (paradox-enable))

(use-package try
  :ensure t
  :commands (try))

(use-package anaphora
  :ensure t)

(use-package f
  :ensure t
  :demand t)

(use-package s
  :ensure t
  :demand t)

(use-package tos
  :ensure nil
  :demand t
  :quelpa
  (tos :repo "pkulev/tos.el"
       :fetcher github :upgrade t))

(use-package infer-indentation-style
  :ensure nil
  :after tos
  :preface
  (defun infer-indentation-style-js ()
    "Sets proper values depending on buffer indentation mode."
    (when (tos-buffer-tabs?)
        (setq indent-tabs-mode t)))

  (defun infer-indentation-style-python ()
    "Sets proper values depending on buffer indentation mode."
    (if (tos-buffer-tabs?)
        (setq indent-tabs-mode t
              python-indent-offset 4
              tab-width 4)))
  (provide 'infer-indentation-style))

(use-package esup
  :ensure t
  :custom
  ;; FIXME: this prevents errors
  (esup-depth 0))

(use-package helpful
  :ensure t
  :demand t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  (:map help-mode-map
        ("f" . helpful-callable)
        ("v" . helpful-variable)
        ("k" . helpful-key)
        ("F" . helpful-at-point)
        ("F" . helpful-function)
        ("C" . helpful-command)))

(use-package free-keys
  :ensure t)

(use-package which-key
  :ensure t
  :defer 2
  :delight
  :config
  (which-key-mode))

(use-package info-colors
  :ensure t
  :hook (Info-selection . info-colors-fontify-node))

(use-package google-translate
  :ensure t
  :bind
  (:map mode-specific-map
        ("t p" . google-translate-at-point)
        ("t P" . google-translate-at-point-reverse)
        ("t t" . google-translate-query-translate)
        ("t T" . google-translate-query-translate-reverse))
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ru"))

(use-package cus-edit
  :ensure nil
  :custom
  (custom-file (concat user-emacs-directory "custom-file.el")))

(use-package my/private-el
  :ensure nil
  :preface
  (defun my/private-el-load ()
    (load (concat user-emacs-directory "private.el") 'noerror))
  (provide 'my/private-el)
  :init
  (my/private-el-load))

(defun display-startup-echo-area-message ())

(use-package emacs
  :ensure nil
  :init
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :hook
  ;; I want to see trailing spaces
  (prog-mode . (lambda () (setq show-trailing-whitespace t)))
  :custom
  (use-dialog-box nil "Dialogs via minibuffer only.")
  (tool-bar-mode nil "Disable toolbar.")
  (menu-bar-mode nil "Disable menubar.")
  (scroll-bar-mode nil "Disable scrollbar.")
  (blink-cursor-mode nil "Disable cursor blinking.")
  (scroll-step 1 "Scroll line by line.")
  (scroll-margin 4 "Top and bottom scrolling margin.")
  (scroll-conservatively 101 "If >100 then never recenter point.")
  (inhibit-splash-screen t "Don't show the splash screen.")
  (initial-scratch-message nil "Disable initial scratch message.")

  (indicate-empty-lines t "Visually indicate empty lines.")
  (indicate-buffer-boundaries 'left "Show buffer boundaries at left fringe.")
  (indent-tabs-mode nil "Tabs are evil.")
  (tab-width 4 "Sane default for me.")
  (read-process-output-max (* 1024 1024) "Increase amount of data read from processes."))

(use-package find-func
  :ensure nil
  :custom
  (find-function-C-source-directory (expand-file-name "~/proj/emacs") "Emacs sources."))

(use-package autorevert
  :ensure nil
  :delight auto-revert-mode)

(use-package frame
  :ensure nil
  :bind
  ("C-z" . nil)
  ("C-c C-z" . nil))

(use-package simple
  :ensure nil
  :delight
  (visual-line-mode)
  :hook ((before-save . delete-trailing-whitespace))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  :custom
  (line-number-mode t "Show line number in modeline.")
  (column-number-mode t "Show column number in modeline.")
  (size-indication-mode t "Show file size in modeline.")
  (global-visual-line-mode t "Enable visual-line-mode."))

(use-package paren
  :ensure nil
  :demand t
  :custom
  (show-paren-delay 0)
  :config
  (show-paren-mode t))

;; TODO: fix somehow different family across OSes
(use-package faces
  :ensure nil
  :config
  (if (eq system-type 'darwin)
      (set-face-attribute 'default
                          nil
                          :family "Fira Code"
                          :weight 'semi-light
                          :width 'semi-condensed
                          :height 130)
    (set-face-attribute 'default
                        nil
                        :family "FiraCode"
                        :weight 'semi-light
                        :width 'semi-condensed
                        :height 130)))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "#3e4446")
  (set-face-foreground 'highlight nil))

(use-package diff-hl
  :ensure t
  :defer t
  :after magit
  :hook
  (prog-mode . diff-hl-mode)
  (org-mode . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package fringe
  :ensure nil
  :custom
  (fringe-mode '(8 . 0)))

(use-package reverse-im
  :ensure t
  :defer 1
  :config
  (reverse-im-activate "russian-computer"))

(use-package fill-column-indicator
  :ensure t
  :custom
  (fci-rule-width 1)
  (fci-rule-color "cadetBlue4")
  (fci-rule-column 80)
  :hook (prog-mode . fci-mode))

(use-package zerodark-theme
  :ensure t
  :demand t
  ;;:after flycheck  ; TODO: make PR for fixing this
  :config
  (load-theme 'zerodark 'noconfirm))
  ;;(zerodark-setup-modeline-format))

(use-package all-the-icons
  :if window-system
  :ensure t
  :config
  (setq all-the-icons-mode-icon-alist
        `(,@all-the-icons-mode-icon-alist
          (package-menu-mode all-the-icons-octicon "package" :v-adjust 0.0))))

(use-package all-the-icons-dired
  :if window-system
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :if window-system
  :ensure t
  :after ivy
  :custom
  (all-the-icons-ivy-buffer-commands '() "Don't use for buffers.")
  :config
  (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
    (all-the-icons-install-fonts t))
  (all-the-icons-ivy-setup))

(use-package time
  :ensure nil
  :custom
  (display-time-mode nil "Don't display time at modeline."))

(use-package nyan-mode
  :ensure t
  :after zerodark-mode
  :custom
  (nyan-bar-length 16)
  :config
  (nyan-mode)
  (zerodark-modeline-setup-format))

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :delight
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character))

(use-package lisp-extra-font-lock
  :ensure t
  :custom
  (lisp-extra-font-lock-modes '(emacs-lisp-mode lisp-mode))
  :config
  (lisp-extra-font-lock-global-mode 1))

(use-package beacon
  ;; TODO: fix animation
  :disabled
  :ensure t
  :defer 5
  :config
  (beacon-mode 1))

(use-package ibuffer
  :ensure nil
  :defer t
  :config
  (defalias 'list-buffers 'ibuffer))

(use-package ace-window
  :ensure t
  :bind ("C-x w" . ace-window))

(use-package dired
  :ensure nil
  :bind ([remap list-directory] . dired)
  :custom
  (dired-recursive-deletes 'top "Confirm deletion for all top non-empty directories.")
  (dired-dwim-target t "Try to guess target for actions."))

(use-package dired-x
  :ensure nil)

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
        ([?\t] . dired-subtree-toggle)))

(use-package dired-hide-dotfiles
  :ensure t
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package image-dired
  :ensure nil)

(use-package image-dired+
  :ensure t
  :after image-dired)

(use-package window
  :ensure nil
  :bind ("M-o" . other-window))

(use-package paragraphs
  :ensure nil
  :preface (provide 'paragraphs)
  :bind (("M-n" . #'forward-paragraph)
         ("M-p" . #'backward-paragraph)))

(use-package imenu
  :ensure nil
  :bind (("C-c C-j" . imenu)
         ("M-i" . imenu))
  :custom
  (imenu-auto-rescan t)
  (imenu-use-popup-menu nil))

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode t))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package copy-as-format
  :ensure t
  :bind
  (:prefix-map
   copy-as-format-prefix-map
   :prefix "C-x c"
   ("f" . copy-as-format)
   ("a" . copy-as-format-asciidoc)
   ("b" . copy-as-format-bitbucket)
   ("d" . copy-as-format-disqus)
   ("g" . copy-as-format-github)
   ("l" . copy-as-format-gitlab)
   ("c" . copy-as-format-hipchat)
   ("h" . copy-as-format-html)
   ("j" . copy-as-format-jira)
   ("m" . copy-as-format-markdown)
   ("w" . copy-as-format-mediawiki)
   ("o" . copy-as-format-org-mode)
   ("p" . copy-as-format-pod)
   ("r" . copy-as-format-rst)
   ("s" . copy-as-format-slack)))

(use-package link-hint
  :ensure t
  :bind
  (:map ctl-x-map
        ("M-l c" . link-hint-copy-link)
        ("M-l o" . link-hint-open-link)
        ("M-l p" . link-hint-open-link-at-point)))

(use-package password-store
  :ensure t)

(use-package vterm
  :ensure t
  :commands (vterm))

(use-package shell
  :ensure nil
  :custom
  (explicit-shell-file-name (executable-find "zsh") "Default inferior shell."))

(use-package eshell-fringe-status
  :ensure t
  :hook
  (eshell-mode . eshell-fringe-status-mode))

(use-package eshell-prompt-extras
  :ensure t
  ;; FIXME: :commands doesn't work
  ; :commands (eshell eshell-toggle)
  :demand t
  :after (eshell esh-opt)
  :custom
  (eshell-prompt-function #'epe-theme-lambda))

(use-package esh-autosuggest
  :ensure t
  :hook
  (eshell-mode . esh-autosuggest-mode))

(use-package esh-help
  :ensure t
  :defer t
  :config
  (setup-esh-help-eldoc))

(use-package eshell-z
  :ensure t
  :after eshell)

(use-package eshell-toggle
  :ensure t
  :bind
  ("M-`" . eshell-toggle)
  :custom
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command "ls"))

(use-package exec-path-from-shell
  :ensure t
  :defer 1
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package files
  :ensure nil
  :custom
  (require-final-newline t)
  (delete-old-versions t)
  (backup-directory-alist
   `((".*" . ,(expand-file-name (concat user-emacs-directory "autosaves/")))))
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name (concat user-emacs-directory "autosaves/")) t))))

(use-package recentf
  :defer 0.1
  :custom
  (recentf-auto-cleanup 30)
  :config
  (recentf-mode)
  (run-with-idle-timer 10 t 'recentf-save-list))

(use-package my-config
  :ensure nil
  :after counsel
  :preface
  (defun my-config-open ()
    (interactive)
    (find-file (concat user-emacs-directory "init.el")))

  (defun my-config-open-readme ()
    (interactive)
    (find-file (concat user-emacs-directory "readme.org")))

  (defun my-config-open-private ()
    (interactive)
    (find-file (concat user-emacs-directory "private.el")))

  (defun my-config-eval ()
    (interactive)
    (load-file (concat user-emacs-directory "init.el")))

  (defun my-config-open-and-search ()
    (interactive)
    (my-config-open)
    (counsel-grep-or-swiper))

  (provide 'my-config)

  :bind
  (:map mode-specific-map
        ("e o" . #'my-config-open)
        ("e r" . #'my-config-open-readme)
        ("e p" . #'my-config-open-private)
        ("e e" . #'my-config-eval)
        ("e s" . #'my-config-open-and-search)))

(use-package prescient
  :ensure t)

(use-package company
  :ensure t
  :delight
  :bind
  (:map company-active-map
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort))
  :hook
  (after-init . global-company-mode))

(use-package company-quickhelp
  :ensure t
  :custom
  (company-quickhelp-delay 3)
  :config
  (company-quickhelp-mode 1))

(use-package company-shell
  :ensure t
  :config
  (add-to-list 'company-backends 'company-shell))

(use-package ag
  :ensure t)

(use-package counsel
  :ensure t
  :delight
  :defer nil
  :bind (([remap menu-bar-open] . counsel-tmm)
         ([remap insert-char] . counsel-unicode-char)
         ([remap isearch-forward] . counsel-grep-or-swiper)
         ([remap isearch-backward] . counsel-grep-or-swiper))
  :config
  (counsel-mode))

(use-package counsel-projectile
  :ensure t
  :after ag counsel projectile
  :bind
  ("C-c p s" . counsel-projectile-ag)
  :config
  (counsel-projectile-mode))

(use-package counsel-dash
  :ensure t
  :after counsel eww
  :requires eww
  :bind
  ;; (:map mode-specific-map ("d i" . counsel-dash-install-docset)
  ;;                         ("d u" . counsel-dash-uninstall-docset))
  ;;                          (""))
  :config
  (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python"))))
  :custom
  (counsel-dash-browser-func 'eww-browse-url))

(use-package swiper
  :ensure t
  :delight
  :defer nil
  :bind
  (([remap isearch-forward-symbol-at-point] . #'swiper-thing-at-point)))

(use-package ivy
  :ensure t
  :delight
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-re-builders-alist '((t . ivy--regex-plus) (t . ivy--regex-fuzzy)))
  (ivy-count-format "%d/%d " "Show anzu-like counter.")
  (ivy-use-selectable-prompt t "Make the prompt line selectable.")
  :custom-face
  (ivy-current-match ((t (:inherit 'hl-line))))
  :bind
  (:map ivy-minibuffer-map
        ("C-r" . ivy-previous-line-or-history))
  :config
  (ivy-mode t))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode))

(use-package ivy-prescient
  :ensure t
  :after ivy prescient
  :config
  (ivy-prescient-mode))

(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-terminal-type "tramp" "This allows to distinguish TRAMP from others.")
  (tramp-default-method "ssh" "SSH is slightly faster that default SCP."))

;; TODO
(use-package counsel-tramp
  :after counsel tramp
  :hook ((counsel-tramp-pre-counsel . (lambda () (projectile-mode 0)))
         (consel-tramp-quit . (lambda () (projectile-mode 1))))
  :bind
  (:map mode-specific-map ("s s" . #'counsel-tramp)))

(use-package sudo-edit
  :ensure t
  :bind
  (:map ctl-x-map
        ("M-s" . #'sudo-edit)))

(use-package docker
  :ensure t
  :bind
  (:map mode-specific-map
        ("d" . docker)))

(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :ensure t
  :defer t)

(use-package yasnippet
  :ensure t
  :defer 2
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package flycheck
  :ensure t
  :delight
  :custom
  (flycheck-clang-language-standard "c++17")
  (flycheck-cppcheck-standards '("c++17"))
  :init (global-flycheck-mode))

(use-package compile
  :ensure nil
  :bind ([f5] . recompile))

(use-package ispell
  :ensure nil)

(use-package smart-comment
  :ensure t
  :bind ("M-;" . smart-comment))

(use-package fixmee
  :ensure t
  :delight
  (button-lock-mode)
  (fixmee-mode)
  :hook (prog-mode . global-fixmee-mode)
  :init (require 'button-lock))

(use-package dotenv
  :ensure nil
  :after projectile
  :quelpa
  (dotenv :repo "pkulev/dotenv.el"
          :fetcher github :upgrade t)
  :config
  (defun dotenv-absolutify-path (path)
    "Make all pathes in PATH absolute using project root."
    (when (s-present? path)
      (let ((root (projectile-project-root)))
        (s-join ":" (mapcar (lambda (it) (f-join root it)) (s-split ":" path))))))

  (defun dotenv-projectile-hook ()
    "Projectile hook."
    (let ((path (dotenv-path (projectile-project-root))))
      (when (s-present? path)
        (dotenv-update-env (dotenv-load path))
        (let ((pythonpath (dotenv-absolutify-path (dotenv-get "PYTHONPATH" path))))
          (when pythonpath
            (setq python-shell-extra-pythonpaths (s-split ":" pythonpath))
            (setenv "PYTHONPATH" pythonpath))))))

  (add-to-list 'projectile-after-switch-project-hook #'dotenv-projectile-hook))

;; TODO: c2 projectile integration
(use-package projectile
  :ensure t
  :defer nil
  :bind
  (:map mode-specific-map ("p" . projectile-command-map))
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode))

(use-package sloc
  :ensure nil
  :quelpa
  (sloc :repo "leoliu/sloc.el"
        :fetcher github :upgrade t))

(use-package po-mode
  :ensure t)

(use-package cc-vars
  :ensure nil
  :hook
  (c-mode-hook . (lambda () (c-set-style "k&r")))
  (c++-mode-hook . (lambda () (c-set-style "k&r")))
  :custom
  (c-basic-offset 4))

(use-package glsl-mode
  :ensure t)

(use-package company-glsl
  :ensure t
  :if (executable-find "glslangValidator")
  :config
  (add-to-list 'company-backends 'company-glsl))

;; TODO: parinfer was removed from MELPA and archived
;; make parinfer-rust-mode work under M1 or use something else like lispy
(use-package parinfer
  :disabled
  :ensure t
  :delight '(:eval (concat " p:" (symbol-name (parinfer-current-mode))))
  :hook ((emacs-lisp-mode . parinfer-mode)
         (common-lisp-mode . parinfer-mode)
         (clojure-mode . parinfer-mode)))

(use-package macrostep
  :ensure t
  :bind
  (:map emacs-lisp-mode-map
        ("C-x m e" . #'macrostep-expand)
        ("C-x m c" . #'macrostep-collapse)
        ("C-x m m" . #'macrostep-mode)))

(use-package elisp-mode
  :ensure nil
  :delight "elisp"
  :commands aorst/emacs-lisp-indent-function
  :hook (emacs-lisp-mode . aorst/emacs-lisp-setup)
  :bind (:map emacs-lisp-mode-map
         ("C-c C-M-f" . aorst/indent-buffer))
  :config
  (defun aorst/emacs-lisp-indent-function (indent-point state)
    "A replacement for `lisp-indent-function'.
Indents plists more sensibly. Adapted from DOOM Emacs:
https://github.com/hlissner/doom-emacs/blob/b03fdabe4fa8a07a7bd74cd02d9413339a485253/modules/lang/emacs-lisp/autoload.el#L91"
    (let ((normal-indent (current-column))
          (orig-point (point))
          target)
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond ((and (elt state 2)
                  (or (not (looking-at-p "\\sw\\|\\s_"))
                      (eq (char-after) ?:)))
             (unless (> (save-excursion (forward-line 1) (point))
                        calculate-lisp-indent-last-sexp)
               (goto-char calculate-lisp-indent-last-sexp)
               (beginning-of-line)
               (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
             (backward-prefix-chars)
             (current-column))
            ((and (save-excursion
                    (goto-char indent-point)
                    (skip-syntax-forward " ")
                    (not (eq (char-after) ?:)))
                  (save-excursion
                    (goto-char orig-point)
                    (and (eq (char-after) ?:)
                         (eq (char-before) ?\()
                         (setq target (current-column)))))
             (save-excursion
               (move-to-column target t)
               target))
            ((let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
                    (method (or (function-get (intern-soft function) 'lisp-indent-function)
                                (get (intern-soft function) 'lisp-indent-hook))))
               (cond ((or (eq method 'defun)
                          (and (null method)
                               (> (length function) 3)
                               (string-match-p "\\`def" function)))
                      (lisp-indent-defform state indent-point))
                     ((integerp method)
                      (lisp-indent-specform method state indent-point normal-indent))
                     (method
                      (funcall method indent-point state))))))))
  (defun aorst/emacs-lisp-setup ()
    (setq-local lisp-indent-function
                #'aorst/emacs-lisp-indent-function))
  (defun org-babel-edit-prep:emacs-lisp (_info)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t)
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

(use-package cider
  :ensure t)

(use-package lisp-mode
  :disabled
  :ensure nil
  :after flycheck
  :hook ((lisp-mode . (lambda () (setq flycheck-enabled-checkers '(sblint)))))
  :config
  (flycheck-define-checker sblint
    "A Common Lisp checker using `sblint'."
    ;; :command ("sblint" source)
    :command ("echo ok" source)
    :error-patterns
    ((error line-start (file-name) ":" line ": error: " (message) line-end))
    :modes lisp-mode)
  (add-to-list 'flycheck-checkers 'sblint))

(use-package sly-asdf
  :ensure t
  :defer t)

(use-package sly-quicklisp
  :ensure t
  :defer t)

(use-package sly
  :ensure t
  :defer t
  :after (sly-asdf sly-quicklisp)
  :custom
  (inferior-lisp-program (executable-find "sbcl")))
;;  (sly-contribs '(sly-asdf sly-quicklisp)))

(use-package geiser
  :ensure t
  :if (executable-find "guile")
  :bind
  ("C-c i" . geiser-insert-lambda)
  :custom
  (geiser-default-implementation 'guile))

(use-package hy-mode
  :ensure t)

(use-package python
  :ensure nil
  :delight python-mode)

(use-package sphinx-doc
  :ensure t
  :delight
  :hook (python-mode . sphinx-doc-mode))

(use-package poetry
  :ensure t
  :config
  (poetry-tracking-mode))

(use-package pyvenv
  :ensure t
  :hook ((python-mode . pyvenv-mode)
         (python-mode . pyvenv-tracking-mode)))

(use-package lsp-mode
  :ensure t
  :delight
  :preface
  ;; TODO: make configurable
  (defun pkulev/pyvenv-autoload ()
    "Automatically activate pyvenv when .venv directory exists."
    (f-traverse-upwards
     (lambda (path)
       (let ((venv-path (f-expand ".venv" path)))
         (if (f-exists? venv-path)
             (progn
               (pyvenv-activate venv-path)
               t))))))
  (defun pkulev/python-setup-indentation ()
    (setq python-indent-def-block-scale 1)
    (infer-indentation-style-python))
  :hook ((python-mode . lsp)
         (python-mode . pkulev/pyvenv-autoload)
         (python-mode . pkulev/python-setup-indentation)))

(use-package lsp-ui
  :ensure t
  :commands (lsp)
  :custom
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-ignore-duplicate t)
  :hook (lsp-mode . company-mode))

(use-package js
  :ensure nil
  :config
  :hook (js-mode . infer-indentation-style-js))

(use-package mhtml-mode
  :ensure nil
  :defer t
  :custom
  (sgml-basic-offset 4))

(use-package nim-mode
  :ensure t
  :hook
  ((nim-mode . nimsuggest-mode)
   (nimsuggest-mode . flycheck-mode)))

(use-package flycheck-nim
  :ensure t
  :after nim-mode)

(use-package tuareg
  :ensure t
  :defer t
  :custom
  (tuareg-match-patterns-aligned t))
;; (tuareg-prettify-symbols-full t)
;; TODO:
;; (add-hook 'tuareg-mode-hook
;;           (lambda()
;;             (when (functionp 'prettify-symbols-mode)
;;               (prettify-symbols-mode))))

;; (face-spec-set
;;  'tuareg-font-lock-constructor-face
;;  '((((class color) (background light)) (:foreground "SaddleBrown"))
;;    (((class color) (background dark)) (:foreground "burlywood1")))))

(use-package racer
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode))
  :custom
  (rust-rustfmt-bin "~/.cargo/bin/rustfmt")
  (rust-cargo-bin "~/.cargo/bin/cargo"))

(use-package terraform-mode
  :ensure t)

(use-package magit
  :ensure t
  :delight
  :custom
  (magit-bury-buffer-function #'quit-window)
  :bind
  (:map mode-specific-map
        :prefix-map magit-prefix-map
        :prefix "m"
        ("b" . #'magit-blame-addition)
        ("B" . #'magit-branch-create)
        ("c" . #'magit-checkout)
        ("C" . #'magit-commit-create)
        ("f" . #'magit-find-file)
        ("l" . #'magit-log-buffer-file)))

(use-package forge
  :if (boundp 'my/private-forges)
  :ensure t
  :delight
  :after magit
  :config
  (add-to-list 'forge-alist
               (append 'my/private-forges forge-github-repository)))

(use-package bookmark
  :ensure nil
  :config
  (when (f-exists? bookmark-default-file)
    (bookmark-load bookmark-default-file t))
  :custom
  (bookmark-save-flag t)
  (bookmark-default-file (f-join user-emacs-directory "bookmarks")))

(use-package bm
  :ensure t
  :bind (("<C-f2>" . bm-toggle)
         ("<f2>"   . bm-next)
         ("<S-f2>" . bm-previous)))

(use-package telega
  :if (> emacs-major-version 25)
  :ensure nil
  :quelpa
  (telega :repo "zevlg/telega.el"
          :fetcher github :upgrade t)
  :load-path "~/proj/telega.el"
  :commands (telega)
  :defer t
  :config
  (add-hook 'telega-root-mode-hook (lambda () (telega-notifications-mode 1))))

(use-package org
  ;; :hook (auto-save . org-save-all-org-buffers)
  :ensure t
  :init
  (defun +org/agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (+org/current-is-todo)
        (setq should-skip-entry t
              (save-excursion
                (while (and (not should-skip-entry) (org-goto-sibling t))
                  (when (+org/current-is-todo)
                    (setq should-skip-entry t))
                  (when should-skip-entry))
                (or (outline-next-heading
                     (goto-char (point-max)))))))))

  (defun +org/current-is-todo ()
    (string= "TODO" (org-get-todo-state)))

  (defun +org/opened-buffer-files ()
    "Return the list of files currently opened in emacs."
    ;; (remove-if-not #'(lambda (x) (string-match "\\.org$" x))
    ;;                   (delq nil (mapcar #'buffer-file-name (buffer-list))))
    (delq nil
          (mapcar (lambda (x)
                    (if (and (buffer-file-name x)
                             (string-match "\\.org$" (buffer-file-name x)))
                        (buffer-file-name x)))
                  (buffer-list))))

  (defun +org/all-org-files ()
    "Return the list of all org files in `org-directory'."

    (remove-if-not #'(lambda (x) (string-match "\\.org$" x))
                   (directory-files org-directory 'full)))

  :ensure org-plus-contrib
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture))
  :custom
  (org-directory "~/orgs")
  (org-log-done 'note)
  (org-log-refile t)
  (org-agenda-files `(,(concat org-directory "/inbox.org")
                      ,(concat org-directory "/next.org")
                      ,(concat org-directory "/tickler.org")))
  ;; (org-refile-targets '((+org/opened-buffer-files :maxlevel . 9)))
  (org-refile-targets '((+org/all-org-files :maxlevel . 9)))
  (org-refile-use-cache t)
  (org-capture-templates
   `(("t" "Todo [inbox]" entry
      (file+headline "/inbox.org" "Tasks")
      "* TODO %i%?")
     ("T" "Tickler" entry (file+headline "/tickler.org" "Tickler")
      "* %i%? \n %U")
     ("P" "Project [projects]" entry
      (file+headline "~/orgs/projects.org", "Projects")
      "* TODO %i%?")
     ("p" "Protocol" entry
      (file+headline "~/orgs/links.org" "Inbox")
      "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
     ("L" "Protocol Link" entry
      (file+headline "~/orgs/links.org" "Inbox")
      "* %? [[%:link][%:description]] \nCaptured On: %U")))
  (org-todo-keywords '((sequence
                        "NEXT(n)" "TODO(t)" "INPROGRESS(p)" "WAITING(w)"
                        "|" "DONE(d)" "CANCELLED(c)")))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  ;; (org-refile-targets '(("~/orgs/next.org" :maxlevel . 3)
  ;;                       ("~/orgs/someday.org" :level . 1)
  ;;                       ("~/orgs/tickler.org" :maxlevel . 2)
  ;;                       ("~/orgs/future-projects.org" :level . 1)))
  (org-agenda-custom-commands
   '(("o" "At the office" tags-todo "@office"
      ((org-agenda-overriding-header "Office")
       (org-agenda-skip-function #'+org/agenda-skip-all-siblings-but-first)))))
  :config
  ;; (run-with-idle-timer 300 t (lambda ()
  ;;                              (org-refile-cache-clear)
  ;;                              (org-refile-get-targets)))
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (python . t)
                               (shell . t)
                               (scheme . t)))
  (add-to-list 'org-structure-template-alist '("ss" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("sp" . "src python"))
  (add-to-list 'org-structure-template-alist '("se" . "src elisp")))

(use-package org-tempo
  :ensure nil)

(use-package org-protocol
  :ensure nil)

(use-package org-bullets
  :ensure t
  :custom
  ;; org-bullets-bullet-list
  ;; default: ◉ ○ ✸ ✿
  ;; large: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; Small: ► • ★ ▸
  ;; (org-bullets-bullet-list '("•"))
  ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱
  ;; (org-ellipsis "…")
  (org-ellipsis "⤵")
  :hook
  (org-mode . org-bullets-mode))

(use-package org-trello
  :ensure t)

(use-package toc-org
  :ensure t
  :hook
  (org-mode . toc-org-mode))

(use-package ob-mongo
  :ensure t)

(use-package ob-async
  :ensure t)

(use-package ox-jira
  :ensure t
  :hook (org-mode . (lambda () (require 'ox-jira))))

(use-package yankpad
  :ensure t
  :defer org
  :bind
  ("C-c y m" . yankpad-map)
  ("C-c y e" . yankpad-expand)
  :config
  (add-to-list 'company-backends #'company-yankpad))

(defun link-message ()
  "Show org-link in minibuffer."
  (interactive)
  (let ((object (org-element-context)))
    (when (eq (car object) 'message)
      (message "%s" (org-element-property :raw-link object)))))

(use-package org-pomodoro
  :ensure nil
  :quelpa
  (org-pomodoro :repo "pkulev/org-pomodoro"
                :fetcher github :branch "feature/customize-mode-line"
                :upgrade t)
  :bind
  (:map mode-specific-map ("o p" . org-pomodoro))
  :custom
  (org-pomodoro-format " 🍅 %s"))

(use-package jira-markup-mode
  :ensure t
  :defer t)

(use-package wakatime-mode
  :ensure t
  :if (boundp 'my/private-wakatime-api-key)
  :delight "👀"
  :custom
  (wakatime-api-key my/private-wakatime-api-key)
  (wakatime-cli-path my/private-wakatime-cli-path)
  :config
  (global-wakatime-mode))

(use-package calendar
  :ensure nil
  :commands (calendar)
  :custom
  (calendar-week-start-day 1))

(use-package org-jira
  :if (boundp 'my/private-jira-url)
  :ensure t
  :custom
  (jiralib-url my/private-jira-url))

;; TODO:
;; https://sourceforge.net/p/confluence-el/wiki/Home/
(use-package confluence
  :if (boundp 'my/private-confluence-url)
  :ensure t
  :defer t
  :custom
  (confluence-url my/private-confluence-url)
  (confluence-default-space-alist (my/private-confluence-default-space)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
