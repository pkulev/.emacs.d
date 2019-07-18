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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
