#!/usr/bin/env bash
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

if grep -q x1cg6 /etc/hostname; then
    # Profile for the X1 Carbon G6 laptop, which may or may not be plugged into the docking station.
    if xrandr | grep -q "DP-1-2 connected"; then
        x1cg6_docked
    else
        x1cg6_undocked
    fi
fi
