#!/usr/bin/env bash
info=$(xwininfo -name $1)
notify-send -u low -t 5000 "$info"
