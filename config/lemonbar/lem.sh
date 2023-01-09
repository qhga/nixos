#!/usr/bin/env bash
scriptdir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
scripts="$scriptdir/scripts"
## Colors
# black=$(xrdb -query | rg -e "\.black:" | cut -f 2)
# red=$(xrdb -query | rg -e "\.red:" | cut -f 2)
# green=$(xrdb -query | rg -e "\.green:" | cut -f 2)
# yellow=$(xrdb -query | rg -e "\.yellow:" | cut -f 2)
# blue=$(xrdb -query | rg -e "\.blue:" | cut -f 2)
# magenta=$(xrdb -query | rg -e "\.magenta:" | cut -f 2)
# cyan=$(xrdb -query | rg -e "\.cyan:" | cut -f 2)
# white=$(xrdb -query | rg -e "\.white:" | cut -f 2)
# # bright
# b_black=$(xrdb -query | rg -e "\.b_black:" | cut -f 2)
# b_red=$(xrdb -query | rg -e "\.b_red:" | cut -f 2)
# b_green=$(xrdb -query | rg -e "\.b_green:" | cut -f 2)
# b_yellow=$(xrdb -query | rg -e "\.b_yellow:" | cut -f 2)
# b_blue=$(xrdb -query | rg -e "\.b_blue:" | cut -f 2)
# b_magenta=$(xrdb -query | rg -e "\.b_magenta:" | cut -f 2)
# b_cyan=$(xrdb -query | rg -e "\.b_cyan:" | cut -f 2)
# b_white=$(xrdb -query | rg -e "\.b_white:" | cut -f 2)

one_color="#ffffff"
one_color_accent="#fdaa3a" # "#000000"
aa=ff # opacity ff = not transparent
bg="#000000$aa"
bg_alt=$one_color
fg=$one_color
fg_alt=$one_color

yellow=$one_color
fg_active_desktop=$one_color_accent
fg_occupied=$one_color
fg_urgent=$one_color
fg_good=$one_color
fg_info=$one_color
fg_warning=$one_color
fg_error=$one_color
fg_last_focused=$one_color

gap=4
x_width=$(xrandr -q | rg -A8 "primary" | rg -o -e "(\d+)x.*\*" -r '$1' || echo 1920)
width=$(($x_width-$gap*2))
height=18

fontmain="Ttyp0:pixelsize=15"
fontglyph="Ttyp0:pixelsize=17"
fontspace="Ttyp0:pixelsize=2"

# mainly wireless for now
_network() {
    local interface icon ssid level
    case $HOSTNAME in
        "killua"|"iArch")
            interface="wlp3s0"
            ssid=$(iwconfig $interface | rg -o -e 'ESSID:"(.+)"' -r '$1')
            level=$(iwconfig $interface | rg -o -e 'level=-(\d+).*' -r '$1')

            if (( level < 70 )); then
                icon="%{F$fg_good}"
            elif (( level >= 70 && level < 80 )); then
                icon="%{F$fg_warning}"
            elif (( level >= 80 )); then
                icon="%{F$fg_error}"
            fi

            echo "%{A:$scripts/network.sh $interface:}%{T2}$icon $ssid%{F-}%{A}";;
        "hisoka")
            interface="en."
            echo "%{A:$scripts/network.sh $interface:}%{T2}%{T-}%{F-}%{A}";;
    esac
}

# Battery status.
_battery() {
    local battery_count percentages percent statuses remaining ac
    ac=""
    if [[ $(acpi battery 2>&1) =~ "No support" ]]
    then
        echo "%{F$yellow}%{T2}$ac%{T-}%{F-}"
        exit
    fi

    readarray -t output <<< $(acpi battery)
    battery_count=${#output[@]}

    for line in "${output[@]}";
    do
        percentages+=($(echo "$line" | rg -o -m1 -e '([0-9]{1,3})%' -r '$1'))
        statuses+=($(echo "$line" | sed 's/Not charging/Unknown/' | rg -o -m1 'Discharging|Charging|AC|Full|Unknown'))
    done

    percent=$(((percentages[0] + percentages[1]) / 2))
    case "${statuses[@]}" in
        *Charging*)
            icon="%{F$yellow}$ac"
            ;;
        *Full*)
            icon=""
            ;;
        *AC*)
            icon="%{F$yellow}$ac"
            ;;
        *Discharging*|*Unknown*)
            if (( percent >= 0 && percent < 5 )); then
                icon="%{F$fg_error}"
                notify-send -u critical ' BATTERY DEAD SOON '
            elif (( percent >= 0 && percent < 10 )); then
                icon="%{F$fg_error}"
            elif (( percent >= 10 && percent < 20 )); then
                icon="%{F$fg_error}"
            elif (( percent >= 20 && percent < 30 )); then
                icon="%{F$fg_warning}"
            elif (( percent >= 30 && percent < 40 )); then
                icon="%{F$fg_warning}"
            elif (( percent >= 40 && percent < 60 )); then
                icon=""
            elif (( percent >= 60 && percent < 70 )); then
                icon=""
            elif (( percent >= 70 && percent < 80 )); then
                icon=""
            elif (( percent >= 80 && percent < 95 )); then
                icon=""
            elif (( percent >= 95 )); then
                icon=""
            fi
            ;;
    esac
    echo "%{A:$scripts/drain.sh:}%{T2}$icon%{T-} $percent%%{F-}%{A}"
}

# Core temperature.
_temperature() {
    # check if acpi is installed
	  command -v acpi > /dev/null || return 1

	  local icon temp

	  icon=""
	  temp="$(acpi -t | awk -F '[[:space:]]+|,' '{ print $5 }')"
	  temp="${temp:0:2}"

	  case "$temp" in
		    [12345][0-9])
			      temp="%{F$yellow}%{T2}$icon%{T-}%{F-} ${temp}°C"
			      ;;
		    [67][0-9])
			      temp="%{F$fg_warning}%{T2}$icon%{T-}%{F-} ${temp}°C"
			      ;;
		    [89][0-9])
			      temp="%{F$fg_error}%{T2}$icon%{T-} ${temp}°C $icon%{F-}"
			      ;;
        *)
			      temp="%{F$fg_error}%{T2}$icon%{F$fg_occupied} %{F-} %{F$fg_error}$icon%{T-}%{F-}"
			      ;;
	  esac


    echo "%{A:$scripts/top5.sh:}$temp%{A}"
}

# Time with button to get Date
_datetime() {
    local d
	  echo "%{A:$scripts/date.sh:}%{T2} %{T-}$(date +"%H:%M")%{A}"
}

# Middle bar - shows current window title
_window_title() {
    local t
    t=$(xtitle)
    [[ "$t" =~ .*qutebrowser.* ]] && t="Surfing the Interwebs"
    echo "${t:0:90}"
}

_rescue_term() {
    echo "%{A:$scripts/terminal.sh:}%{T2}%{T-}%{A}"
}

_to_the_bottom() {
    echo "%{A:$scripts/to_the_bottom.sh:}%{T2}▾ %{T-}%{A}"
}

# Include all modules in a single infinite loop that iterates every
# second (adjust interval accordingly, as it can be taxing on system
# resources).
_modules() {
	  while true; do
		    echo "B" "$(_battery)"
        echo "N" "$(_network)"
		    echo "T" "$(_temperature)"
		    echo "D" "$(_datetime)"
        # echo "S" "$(_tray)"
		    sleep 5s
	  done
}

in_fifo="/tmp/lemon_in.fifo"
out_fifo="/tmp/lemon_out.fifo"

# Delete pipe if it already exists
[ -e "$in_fifo" ] && rm "$in_fifo"
[ -e "$out_fifo" ] && rm "$out_fifo"

# Create new named pipes
mkfifo "$in_fifo"
mkfifo "$out_fifo"

_modules > "$in_fifo" &
bspc subscribe report > "$in_fifo" &

# Read the content of the fifo file.  We differantiate between modules
# based on the majuscule (letter A-Z) they piped into fifo
# (see modules above).  Here we just add a shorter variable to each
# module, which helps position it on the panel (the last printf).
_panel() {

	  while read -r line ; do
		    case $line in
			      # S*)
			      # 	# tray SPACER
			      # 	tr="${line#?}"
			      # 	;;
			      N*)
                # Network
				        net="${line#?}"
				        ;;
			      B*)
				        # battery status
				        bat="${line#?}"
				        ;;
			      T*)
				        # temperature
				        therm="${line#?}"
				        ;;
			      D*)
				        # current date and time
				        date="${line#?}"
				        ;;
			      W*)
				        # bspwm's state
				        wm=
				        IFS=':'
				        set -- ${line#?}
				        while [ "$#" -gt 0 ] ; do
					          item="$1"
					          name="${item#?}"
					          case "$item" in
						            [mMfFoOuULG]*)
							              case "$item" in
								                m*)
									                  # monitor
									                  FG="$fg_alt" # needed to avoid invalid colour error
									                  on_focused_monitor=
									                  name=
									                  ;;
								                M*)
									                  # focused monitor
									                  FG="$fg_alt" # needed to avoid invalid colour error
									                  on_focused_monitor=1
									                  name=
									                  ;;
								                # {Free,Occupied,Urgent} focused
								                [FOU]*)
									                  if [ -n "$on_focused_monitor" ]; then
									                      name=" "
										                    FG="$fg_active_desktop"
									                  else
									                      name=" "
										                    FG="$fg_last_focused"
									                  fi
									                  ;;
								                # {free,occupied,urgent} unfocused
								                f*)
									                  FG="$bg_alt"
									                  name=" "
									                  ;;
								                o*)
									                  FG="$fg_occupied"
									                  name=" "
									                  ;;
								                u*)
									                  FG="$fg_urgent"
									                  name=" "
									                  ;;
								                # desktop layout for monocle and node flags
								                LM|G*?)
								                FG="$fg"
								                name=
								                ;;
								                *)
									                  FG="$fg_alt"
									                  name=
									                  ;;
							              esac
							              wm="${wm}%{F$FG}%{T2}${name}%{T-}%{F-}"
							              ;;
					          esac
					          shift
				        done
		    esac

        # Stuff that runs every time the WM does smth
		    _panel_layout() {
            local left middle right LSPC RSPC

            # left="%{B$bg} $wm%{B-}$RSPC"
            left="%{B$bg} $wm $(_window_title) %{B-}$RSPC"
            # middle="$MLSP%{B$bg} $(_window_title) %{B-}$MRSP"
            right="$LSPC%{B$bg}$(_to_the_bottom) $(_rescue_term) $bat $net $therm $date$tr%{B-}"

			      echo "%{l} $left%{c}$middle%{r}$right  "
		    }
		    printf "%s\n" "%{Sf}$(_panel_layout)"
	  done
}

# Launch the panel with the given parameters
# ------------------------------------------

_panel < "$in_fifo" | lemonbar -u 1 -p -g "${width}x${height}+$gap+$gap" -F "$fg" -B "${bg}" -o 0 -f "$fontmain" -o -0 -f "$fontglyph" -o 0 -f "$fontspace" -n "lemon" >$out_fifo &

sh < "$out_fifo" &

# The -l Flag is important so it is below other windows
trayer --edge top --align center --widthtype request --height 16 --alpha 255 --transparent true --expand false --margin 4 --padding 0 --SetPartialStrut true --SetDockType true --distance 5 -l &

# https://github.com/baskerville/bspwm/issues/484
until bar_id=$(xdo id -a 'lemon'); do
	  sleep 1s
done

xdo below -t $(xdo id -n root) $bar_id &  # Move below bspwm
xdo below -t $(xdo id -n panel) $bar_id & # Move below trayer