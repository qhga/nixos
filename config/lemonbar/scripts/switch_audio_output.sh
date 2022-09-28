#!/usr/bin/env bash
# author: phga
# date: 2020-04-13
# desc: toggle between two pulseaudio sinks
# get sink names
headset=$(pacmd list-sinks | rg -e "name: .*KAVE" -B1 | rg -e ".*index: (\d+)" -r '$1')
h1n=$(pacmd list-sinks | rg -e "name: .*ZOOM" -B1 | rg -e ".*index: (\d+)" -r '$1')
monitor=$(pacmd list-sinks | rg -e "name: .*hdmi" -B1 | rg -e ".*index: (\d+)" -r '$1')
curr_default=$(pacmd list-sinks | rg -e "(\*) index:" | rg -o -e "\d+")

# create new array with active applications
curr_apps=($(pacmd list-sink-inputs | rg -o -e ".*index: (\d+)" -r '$1'))
headset=$h1n
# toggle between sinks
if [ $headset = $curr_default ]; then
    new_default=$monitor
    sink_name="MONITOR"
else
    new_default=$headset
    sink_name="HEADSET"
fi

# move all applications using the old sink to new sink
for app in "${curr_apps[@]}"; do
    pacmd move-sink-input $app $new_default
done

# set new default sink
pacmd set-default-sink $new_default
notify-send -u low -t 750 "New default sink: $sink_name [$new_default]"