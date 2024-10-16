#!/bin/sh

# NOTE: This is only for the live demo, not needed for your configuration!
# spice-vdagent

xrdb ~/.emacs.d/exwm/Xresources
export QT_SCALE_FACTOR=2

picom &

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/desktop.el

