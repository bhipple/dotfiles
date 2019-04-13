#!/usr/bin/env bash
# Configure monitors
x1cg6_docked() {
    xrandr --output DP-1-2 --auto
    xrandr --output eDP-1 --off
    xrandr --dpi 96
}

x1cg6_undocked() {
    xrandr --output eDP-1 --auto
    xrandr --screen 0 --size 2560x1440
    xrandr --dpi 192
}

if grep -q x1cg6 /etc/hostname; then
    # Profile for the X1 Carbon G6 laptop, which may or may not be plugged into the docking station.
    if [[ $(xrandr | grep -cw connected) -eq 2 ]]; then
        x1cg6_docked
    else
        x1cg6_undocked
    fi
fi
