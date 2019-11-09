#!/usr/bin/env zsh
# The kinesis doesn't seem to like comments in the text file, so we strip them.
rm -f out.txt

# Strip comments and blank lines before deployment
sed 's|^\*.*||' qwerty.txt | sed '/^\s*$/d' > out.txt

tgt="/media/usb/keyboard/active/qwerty.txt"
if ! [ -e "$tgt" ]; then
    echo "Compiled this file for deployment:"
    echo "********************************************************************************"
    cat out.txt
    echo "********************************************************************************"
    echo "Kinesis keyboard is not mounted; skipping deployment"
else
    echo "Copied to $tgt"
    sudo cp out.txt "$tgt"
    echo "********************************************************************************"
    cat "$tgt"
    echo "********************************************************************************"
fi
