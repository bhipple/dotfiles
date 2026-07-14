#!/usr/bin/env bash
# Initiailize X. This currently does:
# - Auto initialize monitors and DPI
# - Disable the trackpad
# - Update XResources
undocked() {
    xmodmap ~/dotfiles/xmodmap/colemak
    xrandr --dpi 192 \
           --screen 0 --size 2560x1440 \
           --output eDP-1 --auto --primary --pos 0x0
}

docked() {
    xmodmap ~/dotfiles/xmodmap/qwerty
    # DP-1 is a 5120x2160 (5k2k) panel, 929mm x 392mm => ~140 natural DPI
    xrandr --dpi 140 \
           --output DP-1 --auto --primary --pos 0x0 \
           --output eDP-1 --off
}

handle_touchpad() {
    id=$(xinput list | rg -o 'Synaptics TouchPad.*id=([0-9]*)' -r '$1')
    if [ -n "$id" ]; then
        xinput set-prop "$id" "Device Enabled" 0
    fi
}

handle_dale() {
    xrandr --auto --dpi 144 --output DP-0 --mode 5120x2160
}

xresources() {
    # Apparently this needs to be called at every login!
    xrdb -merge ~/.Xresources 2>&1 >/dev/null
}

xresources

if uname -a | grep -q "Linux brh-x"; then
    # When the 5k2k monitor is plugged in (DP-1 connected), run docked.
    if xrandr --query | grep -q "^DP-1 connected"; then
        docked
    else
        undocked
    fi
    handle_touchpad
elif uname -a | grep -q "Linux brh-dale"; then
    handle_dale
fi
