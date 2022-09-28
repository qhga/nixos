#!/usr/bin/env bash
interface=$1
command -v iwconfig && level=$(iwconfig $interface | rg -o -e 'level=(-\d+).*' -r '$1')
ip_addr=$(ip addr | rg $interface | rg -o -e 'inet (\d+\.\d+\.\d+\.\d+/\d+) ' -r '$1')
dgw=$(ip route | rg $interface | rg -o -e 'default via (\d+\.\d+\.\d+\.\d+) ' -r '$1')

notify-send -u low -t 5000 "Signal:  $level dBm
IP:      $ip_addr
GW:      $dgw"
