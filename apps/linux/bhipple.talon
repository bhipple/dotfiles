-
################################################################################
# General
launch: user.run("dmenu_run")

run alphabet: user.run("alphabet")
run anki: user.run("anki")
run bar: user.run("dmenu_run")
run emacsclient: user.run("emacsclient -c")
run poweroff: user.run("poweroff")
run reboot: user.run("reboot")
run terminal: key(super-enter)

################################################################################
# Org Mode
org agenda: key(f12)
org done: key(ctrl-c ctrl-t d)
org review: key(space o a r)
org goto: key(space o o)

################################################################################
# Emacs

emacs talon: user.run("emacsclient /home/bhipple/.talon/user/knausj_talon/apps/linux/bhipple.talon")

emacs run: key(space r t)
emacs rerun: key(space r r)

emacs buffer: key(space b b)
emacs bookmarks: key(space f b)

# Window management
emacs right: key(space w l)
emacs left: key(space w h)
emacs up: key(space w k)
emacs down: key(space w j)

emacs save: key(escape : w a enter)
emacs close: key(: b d enter)

################################################################################
# XMonad Monitor actions
focus left: key(super-r)
focus right: key(super-s)

xmo throw left: key(super-shift-r)
xmo throw right: key(super-shift-s)

# Throw and follow
xmo follow left: key(super-shift-r super-r)
xmo follow right: key(super-shift-s super-s)

xmo close: key(super-c)
xmo tile: key(super-space)

xmo next: key(super-j)
