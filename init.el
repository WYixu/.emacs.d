(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(scroll-bar-mode -1) ;; Turn off scroll bar
(tool-bar-mode -1) ;; Turn off tool bar
(menu-bar-mode -1) ;; Turn off menu bar
(tooltip-mode -1) ;; Turn off tooltips 
(set-fringe-mode 10) ;; Set left & right blank to 10 pixels

(setq inhibit-startup-message t)

(defun mine/font-settings ()
  (set-face-attribute 'default nil :font "monospace")
  (set-face-attribute 'variable-pitch nil :font "sans")
  (set-fontset-font t 'han "monospace")
  (set-fontset-font t 'kana "monospace")
  (set-fontset-font t 'cjk-misc "monospace")
  (set-fontset-font t 'symbol "monospace"))

(add-hook 'server-after-make-frame-hook #'mine/font-settings) ;; For client mode
(mine/font-settings) ;; For GUI mode

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		eshell-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq enable-recursive-minibuffers t)

(setq read-extended-command-predicate #'command-completion-default-include-p)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https:/orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results nil)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-solarized-light t) ; use solarized light theme

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package nerd-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(keymap-global-set "<escape>" 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode)
  :custom
  (evil-respect-visual-line-mode 1)
  (evil-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(add-hook 'org-mode-hook
          (lambda () (setq evil-auto-indent nil)))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer mine/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC") ;; <C-SPC> is contradict to fcitx5 default settings, should turn off fcitx5 in most cases

  (mine/leader-keys
    "l" '(:ignore t :which-key "ledger-mode")
    "lr" '(ledger-report
	   :which-key "report")
    
    "o" '(:ignore t :which-key "org-mode")
    "oa" '(org-agenda
           :which-key "agenda")
    "or" '(org-redisplay-inline-images
           :which-key "redisplay inline images")
    "ol" '(org-latex-preview
           :which-key "preview LaTeX")
    "oi" '((lambda () (interactive)
             (find-file (concat org-directory "/index.org")))
           :which-key "open index")
    "oc" '(org-capture
	   :which-key "capture")

    "s" '(:ignore t :which-key "start")
    "se" #'(mine/shell-create
	    :which-key "start eshell")

    "t" '(:ignore t :which-key "toggles")
    "tt" '(consult-theme
           :which-key "choose-theme")

    "n" '(:ignore t :which-key "org-roam")
    "nf" '(org-roam-node-find :which-key "find a roam node")
    "ni" '(org-roam-node-insert :which-key "insert a roam node")
    "nl" '((lambda () (interactive)
	      (org-roam-buffer-display-dedicated
	       (org-roam-node-at-point)))
	    :which-key "show backlinks")
    "nd" '(:ignore t :which-key "dailies")
    "ndn" '(org-roam-dailies-capture-today
            :which-key "capture today")
    "ndd" '(org-roam-dailies-goto-today
            :which-key "goto today")
    "ndT" '(org-roam-dailies-capture-tomorrow
            :which-key "capture tomorrow")
    "ndt" '(org-roam-dailies-goto-tomorrow
            :which-key "goto tomorrow")
    "ndY" '(org-roam-dailies-capture-yesterday
            :which-key "capture yesterday")
    "ndy" '(org-roam-dailies-goto-yesterday
            :which-key "goto yesterday")

    "x" '(:keymap perspective-map :package perspective)))

(use-package vertico
  :diminish
  :init
  (vertico-mode 1)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :custom
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  )

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind (("C-x b" . consult-buffer)
	 ("M-p" . consult-project-buffer)
         ("C-s" . consult-line)))

(use-package marginalia
 :init
 (marginalia-mode 1))

(use-package corfu
  :hook
  (prog-mode . corfu-mode)
  (ledger-mode . corfu-mode)
  (eshell-mode . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.0)
  :config
  (keymap-unset corfu-map "RET"))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)
  :custom
  (persp-state-default-file (expand-file-name "persp-state" user-emacs-directory))
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode)
  :config
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (add-hook 'after-init-hook
            (lambda () (persp-state-load persp-state-default-file)))
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source)))

(use-package eglot
  :hook
  (rust-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  :config
  ;; Change the inlay hint face to make it slightly more visible
  (set-face-attribute 'eglot-inlay-hint-face nil :foreground "#8ba34a"))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :custom
  (python-shell-virtualenv-root "~/venv"))

(use-package rust-mode)

(use-package geiser)

(use-package geiser-guile
  :after geiser
  :custom
  (geiser-active-implementations '(guile))
  (geiser-guile-binary "/usr/bin/guile")
  (geiser-repl-autodoc-p t))

(use-package paredit
  :hook (scheme-mode . paredit-mode))

;; (use-package flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode)
;;   (flymake-mode -1)
;;   :custom
;;   (lsp-diagnostics-provider :flycheck))

(use-package origami
  :hook (prog-mode . origami-mode))

(use-package magit
  :commands (magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package eldoc-box
  :config
  (set-face-attribute 'eldoc-box-body nil :font "Sarasa UI CL")
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode))

(use-package eshell
  :custom
  (eshell-scroll-to-bottom-on-input t)
  (eshell-history-size 10000)
  (eshell-save-history-on-exit t)
  (eshell-hist-ignoredups t)
  :config
  (setq-local tab-aways-indent 'complete))

(defun mine/shell-create (name)
   "Create a custom-named eshell buffer with NAME."
   (interactive "sName: ")
   (eshell 'new)
   (let ((new-buffer-name (concat "*eshell-" name "*")))
     (rename-buffer new-buffer-name t)))

(use-package capf-autosuggest
  :hook
  (eshell-mode . capf-autosuggest-mode))

(defun mine/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (display-line-numbers-mode 0)
  ;; (setq evil-auto-intent nil)
  (setq word-wrap-by-category t))

(use-package org
  :hook (org-mode . mine/org-mode-setup)
  :custom
  (org-agenda-files '("~/org/"))
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-refile-targets
   '(("archive.org" :maxlevel . 1)))
  (org-agenda-window-setup 'only-window)
  
  ;; Customs
  (org-agenda-custom-commands
   '(("d" "Dashboard" ;;Agenda Dashboard
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "TODO"
  	     ((org-agenda-overriding-header "All Tasks")))))))

  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/todo.org" "Inbox")
      "* TODO %?")
     ("l" "Literature Note" plain (file "~/org/tmp.org")
      (file "100_Zotero/template.org"))
     ("b" "Toread" entry (file "~/org/booklist.org")
      "* TOREAD %?\n:PROPERTIES:\n:author:\n:rate:\n:END:")))

  (org-preview-latex-default-process 'dvisvgm)
  (org-format-latex-options '(:scale 0.4))
  (org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d!)")
     (sequence "TOREAD" "READING" "|" "READ")))

  :config
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (require 'org-tempo)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cf" . "src conf"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))

(defun mine/org-babel-tangle-config ()
  (when (file-equal-p (file-name-directory (buffer-file-name))
		      (expand-file-name "~/.emacs.d"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'mine/org-babel-tangle-config)))

(defun org-to-clipboard ()
  "Convert the contents of the current buffer or region from Org
mode to HTML.   Store the result in the clipboard."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region (region-beginning)
                               (region-end)
                               "org2clip")
      (shell-command-on-region (point-min)
                               (point-max)
                               "org2clip")))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (concat org-directory "/roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("a" "anime" plain (file "~/org/roam/templates/anime.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Anime\n#+season: %^{Season}\n#+rating: %^{Rating}\n")
      :unnarrowed t)))
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :bind
  (("C-M-i" . org-roam-node-insert))
  :config
  (org-roam-setup)
  )

(use-package org-roam-ui)

(use-package tex
  :ensure auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  :config
  (setq-default TeX-master nil)
  )

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom 
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-agho --group-directories-first")
  )
;;    :config
;; (evil-collection-define-key 'normal 'dired-mode-map
;;      ;;"h" 'dired-up-directory
;; "l" 'dired-find-file))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-gitignore
  :config
  (dired-gitignore-global-mode t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(defun mine/visual-fill-setup ()
  (setq visual-fill-column-width 80
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode))
(use-package visual-fill-column
  :defer t
  :hook ((org-mode LaTeX-mode) . mine/visual-fill-setup))

(use-package edwina
  :config
  (edwina-mode 1))

(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Async Shell Command\\*"
	  org-roam-mode
          helpful-mode
          eshell-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package vterm
  :commands vterm
  :custom
  (vterm-timer-delay nil)
  (vterm-max-scrollback 10000)
  (vterm-shell "/bin/fish")
  :config
  (setq term-prompt-regexp "^❯ *") ;; This works not as intended
  )

(use-package rime
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding))
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-posframe-style 'horizonal)
  (rime-posframe-properties (list :internal-border-width 10
                                :font "Sarasa UI CL Medium")))

(use-package ledger-mode
  :init
  (setq ledger-clear-whole-transactions 1)
  :config
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  :custom
  (ledger-reports
   '(("bal" "%(binary) --strict -f ~/org/PTA/keep.ledger bal")))
  (ledger-accounts-file "~/org/PTA/accounts.ledger")
  :mode "\\.ledger\\'")

(defun mine/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'mine/display-startup-time)
