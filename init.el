(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives
      (append (eval (car (get 'package-archives 'standard-value)))
              '(("org" . "http://orgmode.org/elpa/")
                ("melpa" . "http://melpa.org/packages/")
                ("elpy" . "https://jorgenschaefer.github.io/packages/"))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package)
  (setq use-package-always-ensure t))

(when (getenv "FAST") (setq use-package-always-ensure t))
(when (getenv "STATS") (setq use-package-compute-statistics t))

(use-package bind-key
  :ensure t)

(use-package delight
  :ensure t)

(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t))

(use-package quelpa
  :ensure t
  :custom (quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :ensure t)

(use-package paradox
  :ensure t
  :custom
  (paradox-execute-asynchronously t)
  :config
  (paradox-enable))

(use-package try
  :ensure t)

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . #'helpful-callable)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key)
  ("C-h F" . #'helpful-at-point)
  ("C-h F" . #'helpful-function)
  ("C-h C" . #'helpful-command))

(use-package free-keys
  :ensure t)

(use-package which-key
  :ensure t
  :delight
  :config
  (which-key-mode))

(use-package cus-edit
  :ensure nil
  :after files
  :preface
  (defun load-custom-file ()
    (load custom-file 'noerror))
  (provide 'cus-edit)
  :hook (after-init . #'load-custom-file)
  :config
  (setq custom-file "~/.emacs.d/custom-file.el"))

(use-package emacs
  :ensure nil
  :init
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
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
  (indicate-buffer-boundaries 'left "Show buffer boundaries at left fringe."))

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
  ("C-z" . nil))

(use-package simple
  :ensure nil
  :delight
  (visual-line-mode)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  :custom
  (line-number-mode t "Show line number in modeline.")
  (column-number-mode t "Show column number in modeline.")
  (size-indication-mode t "Show file size in modeline.")
  (global-visual-line-mode t "Enable visual-line-mode."))

(use-package faces
  :ensure nil
  :config
  (set-face-attribute 'default
                      nil
                      :family "FiraCode"
                      :weight 'semi-light
                      :width 'semi-condensed
                      :height 130))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "#3e4446")
  (set-face-foreground 'highlight nil))

(use-package fringe
  :ensure nil
  :custom
  (fringe-mode '(8 . 0)))

(use-package reverse-im
  :ensure t
  :config
  (reverse-im-activate "russian-computer"))

(use-package zerodark-theme
  :ensure t
  :config
  (load-theme 'zerodark 'noconfirm)
  (zerodark-setup-modeline-format))

(use-package all-the-icons
  :ensure t
  :defer t
  :config
  (setq all-the-icons-mode-icon-alist
        `(,@all-the-icons-mode-icon-alist
          (package-menu-mode all-the-icons-octicon "package" :v-adjust 0.0))))

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :ensure t
  :after ivy
  :custom
  (all-the-icons-ivy-buffer-commands '() "Don't use for buffers.")
  :config
  (all-the-icons-ivy-setup))

(use-package time
  :ensure nil
  :custom
  (display-time-mode nil "Don't display time at modeline."))

(use-package nyan-mode
  :ensure t
  :custom
  (nyan-bar-length 16)
  :config
  (nyan-mode))

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :delight
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character))

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
  :config
  (setq dired-recursive-deletes 'top))

(use-package dired-x
  :ensure nil)

;; TODO
(use-package dired-subtree
  :ensure t)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
