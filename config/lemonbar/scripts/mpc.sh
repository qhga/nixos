#!/usr/bin/env bash
case "${1}" in
    "p")
        $(mpc prev);;
    "n")
        $(mpc next);;
    "s")
        $(mpc play);;
    "S")
        $(mpc stop);;
esac
notify-send -u low "ﱘ $(mpc current) ﱘ"
