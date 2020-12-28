app: Emacs
-

################################################################################
# General Emacs
emacs talon:        ":e ~/.talon/user/knausj_talon/bhipple\n"
emacs crafting interpreters: ":e ~/org/notes/crafting-interpreters/crafting-interpreters.org\n"
emacs org: ":e ~/org/me.org\n"

run:                key(space r t)
(rerun|repeat):     key(space r r)

# Jumping from here to there
bookmarks:          " fb"
last:               key(ctrl-6)

buffer$:            " bb"
buffer <user.text>: " bb{text}"


project$:           " pp"
projectile$:        " pp"
project ledger:     " ppledger\n"
projectile there:   " pp"
file$:              " ph"
file <user.text>:   " ph{text}"
projectile here:    " ph"
other file:         " mga"

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

# Editing and coding
comment line:       "gcgc"
line above:         " ik"
line below:         " ij"
undo:               key(escape u)

# Jump to definition with LSP, optionally in another emacs window
find definition$: " mgg"
find definition other$: " mgG"

# Find references to symbol with LSP
find references: " mgr"

find symbol$: " mgs"
find symbol <user.text>: " mgs{text}"

grep$: " g/"
grep point: " g*"

search <user.text>: "/{text}"
search up <user.text>: "?{text}"

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
git commit:         key(ctrl-c ctrl-c)
git fetch:          " gmfa"
git push:           " gmPu"
git refresh:        " !git rf\n"
git status:         " gs"
