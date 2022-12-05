#!/usr/bin/env bash
uname=$(logname)
uhome="/home/$uname"
pwd=$(tree -f $uhome/.password-store | rg -o -e "\.password-store/(.*)\.gpg$" -r '$1' | rofi -dmenu -l 15 -theme-str 'window {width: 50%;}' -i -p)
sudo -u $uname pass -c $pwd &> /dev/null
