#!/usr/bin/env bash
# Initiailize X. This currently does two things:
# 1. Dynamically detect whether we're plugged into the docking station or not, and configure monitors as-needed.
# 2. Disable the
# Configure monitors as-needed depending on whether we're plugged into the
# docking station or not.

x1cg6_docked() {
    xrandr --dpi 96 \
           --output eDP-1 --off \
           --output DP-2-2 --auto --pos 0x900 --primary \
           --output DP-1-2 --auto --pos 2560x0 --rotate left
}

x1cg6_undocked() {
    xrandr --dpi 192 \
           --screen 0 --size 2560x1440 \
           --output eDP-1 --auto --primary --pos 0x0
}

handle_docking() {
    if grep -q x1cg6 /etc/hostname; then
        # Profile for the X1 Carbon G6 laptop, which may or may not be plugged into the docking station.
        if xrandr | grep -q "DP-1-2 connected"; then
            x1cg6_docked
        else
            x1cg6_undocked
        fi
    fi
}

handle_touchpad() {
    id=$(xinput list | rg -o 'Synaptics TouchPad.*id=([0-9]*)' -r '$1')
    if [ -n "$id" ]; then
        xinput set-prop "$id" "Device Enabled" 0
    fi
}

handle_docking
handle_touchpad
