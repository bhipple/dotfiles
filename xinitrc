#!/usr/bin/env bash
# Initiailize X. This currently does two things:
# 1. Dynamically detect whether we're plugged into the docking station or not, and configure monitors as-needed.
# 2. Disable the
# Configure monitors as-needed depending on whether we're plugged into the
# docking station or not.

docked() {
    xmodmap ~/dotfiles_local/xmodmap/qwerty
    dp=$(xrandr | grep '^DP-[1-2] connected' | awk '{print $1}')
    xrandr --dpi 144 \
           --output eDP-1 --off \
           --output $dp --primary --auto

    if grep -q 'HDMI-1 connected'; then
        xrandr --output HDMI-1 --auto --left-of $dp
    fi
}

undocked() {
    xmodmap ~/dotfiles_local/xmodmap/colemak
    xrandr --dpi 192 \
           --screen 0 --size 2560x1440 \
           --output eDP-1 --auto --primary --pos 0x0
}

handle_docking() {
    # Profile for my X1 * laptops, which may or may not be plugged into the docking station.
    if xrandr | grep -q "^DP-[12] connected"; then
        docked
    else
        undocked
    fi
}

handle_touchpad() {
    id=$(xinput list | rg -o 'Synaptics TouchPad.*id=([0-9]*)' -r '$1')
    if [ -n "$id" ]; then
        xinput set-prop "$id" "Device Enabled" 0
    fi
}

xresources() {
    # Apparently this needs to be called at every login!
    xrdb -merge ~/.Xresources 2>&1 >/dev/null
}

xresources
handle_docking
handle_touchpad
