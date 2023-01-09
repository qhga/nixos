#!/usr/bin/env bash
# https://github.com/baskerville/bspwm/issues/484
until bar_id=$(xdo id -a 'lemon'); do
	  sleep 1s
done

xdo below -t $(xdo id -n root) $bar_id &  # Move below bspwm
xdo below -t $(xdo id -n panel) $bar_id & # Move below trayer