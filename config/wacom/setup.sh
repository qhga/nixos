#!/usr/bin/env bash
DISPLAY=":0"
HOME="/home/phga"
XAUTHORITY=$HOME/.Xauthority
DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/1000/bus"

export XAUTHORITY HOME DISPLAY DBUS_SESSION_BUS_ADDRESS

sudo -u phga notify-send "Wacom tablet connected"

i=0
while [ $(xsetwacom --list | wc -l) -lt 1 ] && [ $i -lt 5 ]; do
    sleep 1
    i=$(($i + 1))
done

if [ $(xsetwacom --list | wc -l) -lt 1 ]; then
    sudo -u phga notify-send "xsetwacom found no tablet"
    exit
fi

if [[ "$(xsetwacom --list)" =~ "Intuos" ]]; then
    echo INTOUS
    # LO
    xsetwacom --set "Wacom Intuos PT M 2 Pad pad" Button 3 "key ctrl z"
    # LU
    xsetwacom --set "Wacom Intuos PT M 2 Pad pad" Button 1 "key ctrl s"
    # RO
    xsetwacom --set "Wacom Intuos PT M 2 Pad pad" Button 9 "key ctrl y"
    # RU
    xsetwacom --set "Wacom Intuos PT M 2 Pad pad" Button 8 2
    case $HOSTNAME in
        "killua")
            xsetwacom --set "Wacom Intuos PT M 2 Pen stylus" PressureCurve "0 60 90 100"
            xsetwacom --set "Wacom Intuos PT M 2 Pen stylus" MapToOutput "eDP-1"
            ;;
        "hisoka")
            # xsetwacom --set "HID 256c:006e Pen stylus" PressureCurve "0 60 90 100"
            # xsetwacom --set "HID 256c:006e Pen stylus" MapToOutput "HEAD-0"
            xsetwacom --set "Wacom Intuos PT M 2 Pen stylus" PressureCurve "0 60 90 100"
            # xsetwacom --set "Wacom Intuos PT M 2 Pen stylus" MapToOutput "HEAD-0"
            xsetwacom --set "Wacom Intuos PT M 2 Pen stylus" MapToOutput "2560x1430+0+20"
            # xsetwacom --set "HID 256c:006e stylus" MapToOutput "DP-2"
            ;;
    esac
else
    echo CINTIQ
    # Pad setup
    # BTN: 1
    xsetwacom --set "Wacom Cintiq 13HD Pad pad" Button 2 "key ctrl shift z"
    # BTN: 2
    xsetwacom --set "Wacom Cintiq 13HD Pad pad" Button 3 "key ctrl z"
    # Ring: Middle
    xsetwacom --set "Wacom Cintiq 13HD Pad pad" Button 1 3
    # Ring: Right
    xsetwacom --set "Wacom Cintiq 13HD Pad pad" Button 10 "key 6"
    # Ring: Up
    xsetwacom --set "Wacom Cintiq 13HD Pad pad" Button 11 "key +"
    # Ring: Left
    xsetwacom --set "Wacom Cintiq 13HD Pad pad" Button 12 "key 4"
    # Ring: Down
    xsetwacom --set "Wacom Cintiq 13HD Pad pad" Button 13 "key -"
    # BTN: 3
    xsetwacom --set "Wacom Cintiq 13HD Pad pad" Button 8 "key 2"
    # BTN: 4
    xsetwacom --set "Wacom Cintiq 13HD Pad pad" Button 9 "key shift"

    # Pen setup
    # BTN: Rubber
    # xsetwacom --set "Wacom Cintiq 13HD Pen stylus" Button 1 ""
    # BTN: Bottom
    # xsetwacom --set "Wacom Cintiq 13HD Pen stylus" Button 2 ""
    # BTN: Top
    xsetwacom --set "Wacom Cintiq 13HD Pen stylus" Button 3 "key ctrl"
    # Pressure Curve
    # x y x y    I put my initial pressure pretty high to write effortless for hours (;
    xsetwacom --set "Wacom Cintiq 13HD Pen stylus" PressureCurve "0 20 100 100"

    # Standardwerte vor meiner Kalibrierung
    # Calibrating standard Xorg driver "Wacom Cintiq 13HD Pen stylus"
    #         current calibration values: min_x=400, max_x=59152 and min_y=400, max_y=33448
    xsetwacom --set "Wacom Cintiq 13HD Pen stylus" Area 400 400 59152 33448
    xsetwacom --set "Wacom Cintiq 13HD Pen eraser" MapToOutput $1
    xsetwacom --set "Wacom Cintiq 13HD Pen stylus" MapToOutput $1
fi

# xsetwacom --set "HID 256c:006e Pen stylus" rotate none # half ccw cw
