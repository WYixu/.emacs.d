#!/bin/sh

# NOTE: This is only for the live demo, not needed for your configuration!
# spice-vdagent

xrdb ~/.emacs.d/exwm/Xresources

picom &

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/desktop.el

