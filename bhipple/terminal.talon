app: UXTerm
-

################################################################################
# General Terminal
clear: "c\n"
ls: "ls\n"

# Jumping with zoxide
goto nix packages: "cd ~/src/nixpkgs\n"
goto nixpackages: "cd ~/src/nixpkgs\n"
goto ledger: "cd ~/ledger\n"
goto dotfiles: "cd ~/dotfiles\n"
goto org: "cd ~/org\n"

goto$: "zi\n"
goto <user.text>: "zi\n{text}"

################################################################################
# Ledger
ledger (bal|balance) <user.text>: "l b {text}\n"
ledger (bal|balance)$: "l b\n"

################################################################################
# Nix
borg merge:
     key(b o r g)
     key(space)
     key(shift+insert)

nix shell: "nix-shell\n"
nix build: "nix-build\n"

make build: "make build -j\n"