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
# Org Mode
org agenda: key(f12)

org done: key(ctrl-c ctrl-t d)
org archive: key(ctrl-c shift-4)

org review: key(space o a r)

org file: key(space o o)
org weekly review: key(space o b w)
org agenda: " oaha"

################################################################################
# Emacs

emacs talon:                             key(space o v e)

emacs run:                               key(space r t)
emacs rerun:                             key(space r r)

# Jumping from here to there
emacs bookmarks:                         " fb"
emacs buffer:                            " bb"
emacs last:                              key(ctrl-6)
emacs projectile there:                  " pp"
emacs projectile here:                   " ph"

# Window management
emacs right:                             " wl"
emacs left:                              " wh"
emacs up:                                " wk"
emacs down:                              " wj"
emacs swap:                              " ww"

emacs split:                             ":sp\n"
emacs vest split:                        ":vsp\n"

emacs save:                              key(escape : w a enter)
emacs close:                             key(: b d enter)

################################################################################
# Magit
magit status: key(space g s)
magit commit: key(ctrl-c ctrl-c)

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
