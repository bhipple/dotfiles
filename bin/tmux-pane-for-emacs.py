#!/usr/bin/env python3
import subprocess


def sh(cmd):
    return subprocess.check_output(cmd.split(), text=True).splitlines()


panes = sh('tmux list-panes')
if len(panes) == 1:
    print('0')
else:
    print('1')
