;; -*- lexical-binding: t -*-
;; This file was tangled (automatically generated) from `readme.org'

(require 'package)

(setq package-archives
      (append (eval (car (get 'package-archives 'standard-value)))
              '(("melpa-stable" . "http://stable.melpa.org/packages/")
                ("melpa-unstable" . "http://melpa.org/packages/"))))

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
  :ensure nil
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
  :bind
  (:map help-mode-map
        ("f" . helpful-callable)
        ("v" . helpful-variable)
        ("k" . helpful-key)
        ("F" . helpful-at-point)
        ("F" . helpful-function)
        ("C" . helpful-command)))

(use-package tldr
  :ensure t
  :commands (tldr))

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
  (scroll-step 1 "Scroll line by line.")
  (scroll-margin 4 "Top and bottom scrolling margin.")
  (scroll-conservatively 101 "If >100 then never recenter point.")
  (inhibit-splash-screen t "Don't show the splash screen.")
  (initial-scratch-message nil "Disable initial scratch message.")
  (ring-bell-function 'ignore "Disable the bell ring.")

  (use-dialog-box nil "Dialogs via minibuffer only.")
  (context-menu-mode 1 "Enable context menu.")
  (enable-recursive-minibuffers t "Allow nesting minibuffers.")
  (read-extended-command-predicate #'command-completion-default-include-p
                                   "Hide commands do not work in the current mode.")
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

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

(use-package faces
  :ensure nil
  :config
  (set-face-attribute 'default
                      nil
                      :family (if (memq system-type '(darwin windows-nt)) "Fira Code" "FiraCode")
                      :weight 'semi-light
                      :width 'semi-condensed
                      :height 130))

(use-package hl-line
  :ensure nil
  :defer 1
  :config
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "#EEEEEE")
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

(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-column 100)
  :custom-face
  (fill-column-indicator ((t (:foreground "VioletRed2"))))
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package doom-themes
  :ensure t
  :demand t
  :config
  (load-theme 'doom-one-light 'noconfirm)
  ;; NOTE: this theme overrides doc-face. I like non-italic documentation strings, they must be
  ;;       visible for a programmer, not hidden!
  (set-face-attribute 'font-lock-doc-face nil :slant 'normal))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :ensure t
  :init
  (defun my/dired-subtree-add-nerd-icons ()
    "Add nerd icons into subtree."
    (interactive)
    (revert-buffer))

  (defun my/dired-subtree-toggle-nerd-icons ()
    (when (require 'dired-subtree nil t)
      (if nerd-icons-dired-mode
          (advice-add #'dired-subtree-toggle :after #'my/dired-subtree-add-nerd-icons)
        (advice-remove #'dired-subtree-toggle #'my/dired-subtree-add-nerd-icons))))

  :hook
  (dired-mode . nerd-icons-dired-mode)
  (nerd-icons-dired-mode . my/dired-subtree-toggle-nerd-icons))

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

(use-package form-feed
  :ensure t
  :defer 5
  :delight
  :config
  (global-form-feed-mode))

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

(use-package puni
  :ensure t
  :hook (((fennel
           lua
           ielm-mode
           hy-mode
           inferior-hy-mode
           python-mode
           scheme-mode
           inferior-python-mode
           common-lisp-modes-mode
           clojure-mode
           emacs-lisp-mode
           inferior-emacs-lisp-mode
           go-mode
           nxml-mode) . puni-mode)
         (puni-mode . electric-pair-mode))
  ;; paredit-like keys
  :bind ( :map puni-mode-map
          ("C-M-f" . puni-forward-sexp-or-up-list)
          ("C-M-b" . puni-backward-sexp-or-up-list)
          ("C-M-t" . puni-transpose)
          ;; slurping & barfing
          ("C-<left>" . puni-barf-forward)
          ("C-}" . puni-barf-forward)
          ("C-<right>" . puni-slurp-forward)
          ("C-)" . puni-slurp-forward)
          ("C-(" . puni-slurp-backward)
          ("C-M-<left>" . puni-slurp-backward)
          ("C-{" . puni-barf-backward)
          ("C-M-<right>" . puni-barf-backward)
          ;; depth chaning
          ("M-r" . puni-raise)
          ("M-s" . puni-splice)
          ("M-<up>" . puni-splice-killing-backward)
          ("M-<down>" . puni-splice-killing-forward)
          ("M-(" . puni-wrap-round)
          ("M-{" . puni-wrap-curly)
          ("M-?" . puni-convolute)
          ("M-S" . puni-split))
  :preface
  (define-advice puni-kill-line (:before (&rest _) back-to-indentation)
    "Go back to indentation before killing the line if it makes sense to."
    (when (looking-back "^[[:space:]]*" nil)
      (if (bound-and-true-p indent-line-function)
          (funcall indent-line-function)
        (back-to-indentation)))))

(use-package puni
  :when window-system
  :bind ( :map puni-mode-map
          ;; doesn't work in terminal
          ("M-[" . puni-wrap-square)))

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
  :commands (vterm)
  :custom
  (vterm-max-scrollback 10000))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :bind
  (:map ctl-x-map
        ("C-v" . vterm)
        ("v" . vterm-toggle)
        ("p" . vterm-toggle-backward)
        ("n" . vterm-toggle-forward)))

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
  (eshell-toggle-find-project-root-package 'project "Use built-in project package.")
  (eshell-toggle-run-command "ls"))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac x ns))
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
    (isearch-forward))

  (provide 'my-config)

  :bind
  (:map mode-specific-map
        ("e o" . #'my-config-open)
        ("e r" . #'my-config-open-readme)
        ("e p" . #'my-config-open-private)
        ("e e" . #'my-config-eval)
        ("e s" . #'my-config-open-and-search)))

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

(use-package vertico
  :ensure t
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; NOTE: Marginalia must be activated in the :init section of use-package.
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)

   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g r" . consult-grep-match)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find)                  ;; Alternative: consult-fd
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package consult-dash
  :ensure t
  :bind
  (("M-s D" . consult-dash))
  :config
  (consult-customize consult-dash :initial (thing-at-point 'symbol))
  (add-hook 'python-mode-hook (lambda () (setq-local consult-dash-docsets '("Python 3" "NumPy")))))

(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-terminal-type "tramp" "This allows to distinguish TRAMP from others.")
  (tramp-default-method "ssh" "SSH is slightly faster that default SCP."))

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

(use-package eldoc
  :ensure nil
  :delight)

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
  (flycheck-emacs-lisp-load-path 'inherit)
  :init (global-flycheck-mode))

(use-package compile
  :ensure nil
  :bind ([f5] . recompile))

(use-package ispell
  :ensure nil)

(use-package smart-comment
  :ensure t
  :bind ("M-;" . smart-comment))

(use-package hl-todo
  :ensure t
  :defer 2
  :custom
  (hl-todo-keyword-faces '(("TODO"   . "#FF4500")
                           ("FIXME"  . "#FF0000")
                           ("XXXX*"  . "#FF0000")
                           ("NOTE"   . "#A9A9A9")
                           ("DEBUG"  . "#1E90FF")
                           ("STUB"   . "#A020F0")))
  :init (global-hl-todo-mode))

(use-package dotenv
  :ensure nil
  :demand t
  :quelpa
  (dotenv :repo "pkulev/dotenv.el"
          :fetcher github :upgrade t)
  :init
  (defun dotenv-absolutify-pythonpath (k v)
    (list k (dotenv-absolutify-path-var-in-project v)))

  :config
  (add-to-list
   'dotenv-transform-alist
   '((lambda (k v) (string= k "PYTHONPATH")) . dotenv-absolutify-pythonpath))

  :hook
  (prog-mode . (lambda () (dotenv-update-current-env t))))

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep "Use as project search backend."))

(use-package project
  :ensure nil
  :bind
  ([remap project-vc-dir] . magit-status)
  :bind-keymap
  (("C-c p" . project-prefix-map))
  :custom
  (project-mode-line t)
  (project-switch-commands 'project-find-file "Default command to run after switching."))

(use-package compile
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package sloc
  :ensure nil
  :quelpa
  (sloc :repo "leoliu/sloc.el"
        :fetcher github :upgrade t))

(use-package po-mode
  :ensure t)

(use-package cc-mode
  :ensure nil
  :preface
  (c-add-style "my-c-style"
               '("k&r"
                 (c-basic-offset . 4)
                 (c-offsets-alist . ((case-label . 0)
                                     (inlambda . 0)
                                     (inline-open . 0)
                                     (innamespace . 4)))))
  :hook
  (c-mode-common-hook . (lambda () (c-set-style "my-c-style"))))

(use-package meson-mode
  :ensure t)

(use-package glsl-mode
  :ensure t)

(use-package company-glsl
  :ensure t
  :if (executable-find "glslangValidator")
  :config
  (add-to-list 'company-backends 'company-glsl))

(use-package go-mode
  :ensure t)

(use-package gdscript-mode
  :ensure t
  :custom
  (gdscript-use-tab-indents nil "Gosh."))

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

(use-package flycheck-package
  :ensure t
  :hook (emacs-lisp-mode . flycheck-package-setup))

(use-package cider
  :ensure t)

(use-package sly
  :ensure t
  :defer t
  :custom
  (inferior-lisp-program (executable-find "sbcl")))

(use-package geiser
  :ensure t
  :if (or (executable-find "guile") (executable-find "chicken"))
  :bind
  ("C-c i" . geiser-insert-lambda)
  :custom
  (geiser-default-implementation 'guile))

(use-package geiser-chicken
  :ensure t
  :if (executable-find "chicken")
  :delight "scm/ch")

(use-package hy-mode
  :ensure t)

(use-package fennel-mode
  :ensure t)

(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-level 4 "3 is too uncommon for me."))

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
  :custom
  ;; Disable old and ineffective linters, use all of the ruff!
  (lsp-pylsp-plugins-pylint-enabled nil "Disable pylint.")
  (lsp-pylsp-plugins-flake8-enabled nil "Disable flake8.")
  (lsp-pylsp-plugins-pydocstyle-enabled nil)
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
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (gdscript-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (python-mode . pkulev/pyvenv-autoload)
         (python-mode . pkulev/python-setup-indentation)))

(use-package lsp-ui
  :ensure t
  :commands (lsp)
  :custom
  (lsp-ui-sideline-ignore-duplicate t)
  :hook (lsp-mode . company-mode))

(use-package restclient
  :ensure t)

(use-package restclient-jq
  :ensure t)

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

(use-package ssh-agency
  :if (eq system-type 'windows-nt)
  :init
  (setenv "SSH_ASKPASS" "git-gui--askpass"))

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
  :pin melpa-stable
  :ensure nil
  :commands (telega)
  :defer t
  :config
  (add-hook 'telega-root-mode-hook (lambda () (telega-notifications-mode 1))))

(use-package doc-view
  :ensure nil
  :config
  (doc-view-continuous t)
  (doc-view-resolution 300))

;; TODO: fix autocomplete for loading local file (`l' key)
(use-package mentor
  :ensure t
  :when (executable-find "rtorrent"))

(use-package org
  ;; :hook (auto-save . org-save-all-org-buffers)
  :pin gnu
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

(use-package org-protocol
  :ensure nil)

(use-package org-tempo
  :ensure nil
  :after org-mode)

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

(use-package ob-restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

(use-package ox-jira
  :ensure t
  :hook (org-mode . (lambda () (require 'ox-jira))))

(use-package ox-gfm
  :ensure t
  :defer t)

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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
