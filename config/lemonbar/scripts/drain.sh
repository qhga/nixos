#!/usr/bin/env bash
b0=$(cat /sys/class/power_supply/BAT0/power_now)
b1=$(cat /sys/class/power_supply/BAT1/power_now)

notify-send -u low "Currently consuming: $(awk -v B=$(($b0+$b1)) 'BEGIN {printf "%.4f W", B*10^-6}')"
# echo $drain
