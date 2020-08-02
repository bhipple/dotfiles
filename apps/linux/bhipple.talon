-
################################################################################
# General
launch: user.run("dmenu_run")

run alphabet: user.run("alphabet")
run anki: user.run("anki")
run bar: user.run("dmenu_run")
run poweroff: user.run("poweroff")
run emacsclient: user.run("emacsclient -c")
run terminal: key(super-enter)

################################################################################
# Org Mode
org agenda: key(f12)
org done: key(ctrl-c ctrl-t d)

################################################################################
# Emacs

# Window management
emacs right: key(space w l)
emacs left: key(space w h)
emacs up: key(space w k)
emacs down: key(space w j)

################################################################################
# XMonad Monitor actions
focus left: key(super-r)
focus right: key(super-s)
xmo left: key(super-r)
xmo right: key(super-s)

xmo close: key(super-c)
xmo tile: key(super-space)

xmo 1: key(super-1)
xmo 2: key(super-2)
xmo 3: key(super-3)
xmo 4: key(super-4)
xmo 5: key(super-5)
xmo 6: key(super-6)
xmo 7: key(super-7)
xmo 8: key(super-8)
xmo 9: key(super-9)
