-
################################################################################
# General
launch: user.run("dmenu_run")

run alphabet: user.run("alphabet")
run anki: user.run("anki")
run bar: user.run("dmenu_run")
run chromium: user.run("chromium")
run emacsclient: user.run("emacsclient -c")
run poweroff: user.run("poweroff")
run reboot: user.run("reboot")
run terminal: key(super-enter)

################################################################################
# Terminal
clear: "c\n"

################################################################################
# Ledger
ledger bal <user.text>: "l b {text}\n"
ledger bal$: "l b\n"
ledger balanace$: "l b\n"
ledger balance <user.text>: "l b {text}\n"

################################################################################
# XMonad Monitor actions
focus left: key(super-r)
focus right: key(super-s)

xmo throw left: key(super-shift-r)
xmo throw right: key(super-shift-s)
xmo throw <number>$: key(super-shift-number)
xmo throw 1: key(super-shift-1)

# Throw and follow
xmo follow left: key(super-shift-r super-r)
xmo follow right: key(super-shift-s super-s)

xmo close: key(super-c)
xmo tile: key(super-space)

xmo next: key(super-j)
