#!/usr/bin/env sh
!(emacsclient -c $@) && $(/bin/emacs --daemon) && emacsclient -c $@
