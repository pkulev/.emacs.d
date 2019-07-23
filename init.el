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

(use-package imenu
  :ensure nil
  :bind ("C-c C-j" . imenu)
  :config
  (setq imenu-auto-rescan t)
  (setq imenu-use-popup-menu nil))

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)))

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
  (("C-x M-l o" . link-hint-open-link)
   ("C-c M-l c" . link-hint-copy-link)))

(use-package shell
  :ensure nil
  :custom
  (explicit-shell-file-name "/bin/zsh" "Default inferior shell."))

(use-package shell-pop
  :ensure t
  :bind (("C-`" . shell-pop)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package files
  :ensure nil
  :custom
  (backup-directory-alist
   `((".*" . ,(concat user-emacs-directory "autosaves/"))))
  (auto-save-file-name-transforms
   `((".*" ,(concat user-emacs-directory "autosaves/") t))))

(use-package my-config-mode
  :ensure nil
  :preface
  (defun my-config-open ()
    (interactive)
    (find-file (concat user-emacs-directory "init.el")))

  (defun my-config-eval ()
      (interactive)
    (load-file (concat user-emacs-directory "init.el")))

  (provide 'my-config-mode)

  :bind
  (:map mode-specific-map
	("e o" . #'my-config-open)
	("e e" . #'my-config-eval)))

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

(use-package counsel
  :ensure t
  :delight
  :defer nil
  :bind (([remap menu-bar-open] . counsel-tmm)
	 ([remap insert-char] . counsel-unicode-char)
	 ([remap isearch-forward] . counsel-grep-or-swiper))
  :config
  (counsel-mode))

(use-package counsel-projectile
  :ensure t
  :after counsel projectile
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
  (:map mode-specific-map ("C-r" . ivy-resume))
  :config
  (ivy-mode t))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode))

(use-package telega
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
      (unless (+org/current-is-todo))
     (setq should-skip-entry t
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (+org/current-is-todo)
            (setq should-skip-entry t))
          (when should-skip-entry))
        (or (outline-next-heading
             (goto-char (point-max))))))))

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

(use-package org-tempo)

(use-package org-protocol)

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

(use-package ob-mongo
  :ensure t)

(use-package ob-async
  :ensure t)

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
  :config (setq calendar-week-start-day 1))

(use-package telega
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
      (unless (+org/current-is-todo))
     (setq should-skip-entry t
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (+org/current-is-todo)
            (setq should-skip-entry t))
          (when should-skip-entry))
        (or (outline-next-heading
             (goto-char (point-max))))))))

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

(use-package org-tempo)

(use-package org-protocol)

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

(use-package ob-mongo
  :ensure t)

(use-package ob-async
  :ensure t)

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
  :config (setq calendar-week-start-day 1))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
