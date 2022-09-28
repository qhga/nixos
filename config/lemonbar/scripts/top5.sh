#!/usr/bin/env bash
notify-send -u low -t 5000 "$(ps -Ao user,comm,pcpu,pmem --sort=-pcpu | head -n 6)"
