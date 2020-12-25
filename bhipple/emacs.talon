app: Emacs
-

################################################################################
# General Emacs
emacs talon:        ":e ~/.talon/user/knausj_talon/bhipple\n"

run:                key(space r t)
(rerun|repeat):     key(space r r)

# Jumping from here to there
bookmarks:          " fb"
buffer:             " bb"
last:               key(ctrl-6)

project$:           " pp"
projectile$:        " pp"
project ledger:     " ppledger\n"
projectile there:   " pp"
files:              " ph"
projectile here:    " ph"

# Window management
right:              " wl"
left:               " wh"
up:                 " wk"
down:               " wj"
swap:               " ww"

split:              ":sp\n"
vest split:         ":vsp\n"

save:               key(escape : w a enter)
close:              key(: b d enter)

# Editing
comment line:       "gcgc"

################################################################################
# Org Mode
org done:           key(ctrl-c ctrl-t d)
org archive:        key(ctrl-c shift-4)

org review:         key(space o a r)

org (file|home):    key(space o o)
org weekly review:  key(space o b w)
org agenda:         " oaha"

################################################################################
# Magit
git status:             key(space g s)
commit:             key(ctrl-c ctrl-c)
