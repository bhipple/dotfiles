#!/usr/bin/env zsh
set -euo pipefail
tgt="/media/usb/keyboard/active/qwerty.txt"

mount() {
    if ! lsblk -f | grep -q ADVANTAGE2; then
        echo "Kinesis is not plugged in"
        exit 1
    fi

    device=$(lsblk -f | grep ADVANTAGE2 | awk '{print $1}')
    sudo mount /dev/$device /media/usb/keyboard

    echo "Mounted Kinesis"
}

deploy() {
    # The kinesis doesn't seem to like comments in the text file, so we strip them.
    rm -f out.txt

    # Strip comments and blank lines before deployment
    sed 's|^\*.*||' qwerty.txt | sed '/^\s*$/d' > out.txt

    if grep '^[^\[{]' out.txt; then
        echo "Syntax error! Every line must start with [ or {"
        exit 1
    fi

    if ! [ -e "$tgt" ]; then
        echo "Compiled this file for deployment:"
        echo "********************************************************************************"
        cat out.txt
        echo "********************************************************************************"
        echo "Kinesis keyboard is not mounted; skipping deployment"
    else
        echo "********************************************************************************"
        echo "Copying to $tgt with this diff:"
        diff out.txt "$tgt" || true
        sudo cp out.txt "$tgt"
        echo "********************************************************************************"
        cat "$tgt"
        echo "********************************************************************************"
    fi

    sudo umount /media/usb/keyboard
    echo "Unmounted Kinesis"
}

mount
deploy
