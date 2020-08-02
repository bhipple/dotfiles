-
################################################################################
# General
launch: user.launch()
alphabet: user.alphabet()

################################################################################
# Org Mode
org agenda: key(f12)
org done: key(ctrl-c ctrl-t d)

################################################################################
# Emacs
emacs client: emacsclient()

# Window management
emacs right: key(space w l)
emacs left: key(space w h)
emacs up: key(space w k)
emacs down: key(space w j)

################################################################################
# XMonad Monitor actions
focus left: key(super-r)
focus right: key(super-s)