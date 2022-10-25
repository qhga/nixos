#!/usr/bin/env sh
# This scrip and the way the udev rule is setup are - to my current knowledge -
# absolutely necessary. Otherwise xsetwacom --list won't return anything
# I tried every possible variation but only this one worked
#
# UDEV RULE:
#
# services.udev.extraRules = ''
#     ACTION=="add",SUBSYSTEM=="usb",ATTRS{idVendor}=="056a",ATTRS{idProduct}=="033e",RUN+="${pkgs.bash}/bin/bash ${dotf}/config/wacom/at.sh"
#   '';
PATH="/run/wrappers/bin:/run/current-system/sw/bin:$PATH"
echo "/home/phga/.dotfiles/config/wacom/setup.sh &" | at now