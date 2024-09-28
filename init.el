(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(setq inhibit-startup-message t)

(set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font Propo")
(set-face-attribute 'variable-pitch nil :font "Noto Sans CJK SC")
(set-fontset-font t 'han "Noto Sans CJK SC")
(set-fontset-font t 'kana "Noto Sans CJK JP")
(set-fontset-font t 'cjk-misc "Noto Sans CJK JP")

(setq spilt-width-threshold 0)
(setq spilt-height-threshold nil)

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-oksolar-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package nerd-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun mine/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 1)
  (display-line-numbers-mode 0)
  (setq evil-auto-intent nil))

(use-package org
  :hook (org-mode . mine/org-mode-setup)
  :custom
  ;; ==============
  ;; === Agenda ===
  ;; ==============
  ;; Basic Setting
  (org-agenda-files
   '("~/Documents/Notes/"))
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-refile-targets
   '(("archive.org" :maxlevel . 1)))

  ;; Customs
  (org-agenda-custom-commands
   '(("d" "Dashboard" ;;Agenda Dashboard
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "TODO"
	     ((org-agenda-overriding-header "All Tasks")))))))

  :config
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; =============
  ;; === Habit ===
  ;; =============
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ;; =============
  ;; === Tempo ===
  ;; =============
  (require 'org-tempo)
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-structure-template-alist
		 '("el" . "src emacs-lisp"))

  ;; =============
  ;; === Babel ===
  ;; =============
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

(defun mine/org-babel-tangle-config ()
  (when (file-equal-p (buffer-file-name)
		      (expand-file-name "~/.emacs.d/init.org"))
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'mine/org-babel-tangle-config)))

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
  :hook ((org-mode latex-mode) . mine/visual-fill-setup))

(use-package rime
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-posframe-style 'vertical)
  (rime-posframe-properties (list :internal-border-width 10
				  :font "Noto Sans CJK SC Bold")))

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

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer mine/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (mine/leader-keys
    "o" '(:ignore o :which-key "Org-mode")
	"oa" '(org-agenda :which-key "Org-agenda")))
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose-theme")
