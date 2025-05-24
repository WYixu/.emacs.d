#!/bin/sh

# Load dpi settings
xrdb ~/.emacs.d/exwm/Xresources
export QT_SCALE_FACTOR=2

# Start compositor
picom &

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/exwm/desktop.el
