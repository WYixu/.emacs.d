(defun mine/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun mine/exwm-update-title ()
  (pcase exwm-class-name
    ("firefox-esr" (exwm-workspace-rename-buffer (format "Firefox: %s" (substring exwm-title 0 (min 30 (length exwm-title))))))))

(defun mine/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("firefox-esr" (exwm-workspace-move-window 2))
    ("mpv" (exwm-workspace-move-window 6))
    ))

(defun mine/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun mine/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  ;;(eshell)
  (display-battery-mode 1)

  (setq display-time-day-and-date t)
  (setq display-time-24hr-format t)
  (setq display-time-interval 1)
  (setq display-time-format "%b %d %H:%M")
  (setq display-time-mail-string "")
  (setq display-time-default-load-average nil)
  (display-time-mode 1)

  ;; Launch apps that will run in the background
  (mine/run-in-background "nm-applet")
  (mine/run-in-background "/home/yxwang/Downloads/clash-verge_1.6.5_amd64.AppImage")
  (mine/run-in-background "touchegg")
  (mine/run-in-background "blueman-applet")
  (mine/run-in-background "pasystray")
  (mine/run-in-background "suwayomi-server")
  )

(defun mine/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil  "feh --bg-scale /home/yxwang/Pictures/miki_240728_01.jpg"))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 10)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'mine/exwm-update-class)

  (add-hook 'exwm-update-title-hook #'mine/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'mine/configure-window-by-class)

  ;; When EXWM start up, do some extra configuration
  (add-hook 'exwm-init-hook #'mine/exwm-init-hook)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-d") 'counsel-linux-app)

  ;; Display all EXWM buffers in every workspace buffer list
  (setq exwm-workspace-show-all-buffers t)

  ;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)
  (setq exwm-workspace-minibuffer-position 'bottom)

  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output eDP --primary --mode 2880x1800 --pos 0x0 --rotate normal")
  (mine/set-wallpaper)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; Edit text in an *exwm-edit* buffer
  (use-package exwm-edit
    :config
    (add-to-list 'exwm-input-global-keys '([?\C-c ?\'] . exwm-edit--compose)))

  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))
