## ============================================================================
##                                 Settings
## ============================================================================
# Vim mode
bindkey -v

# Command auto-correction.
ENABLE_CORRECTION="true"

# Disable marking untracked files under VCS as dirty. This makes repository
# status check for large repositories much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

visualModes=("visual")

# Run `bindkey -l` to see a list of modes, and `bindkey -M foo` to see a list of
# commands active in mode foo

# zsh-autosuggestions cfg
# Bind <CTRL><SPC> to accept and execute
bindkey '^ ' autosuggest-execute

# Shift-tab to cycle backwards in autocompletions
bindkey '^[[Z' reverse-menu-complete

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^e' edit-command-line
bindkey -M "vicmd" '^e' edit-command-line

export NIXPKGS_ALLOW_UNFREE=1

# Similar to the rename command
# http://www.gsp.com/cgi-bin/man.cgi?section=1&topic=zshcontrib
autoload -U zmv

#  ============================================================================
#                          Text Object Surround
#  ============================================================================
autoload -Uz surround
zle -N add-surround surround
zle -N change-surround surround
zle -N delete-surround surround

bindkey -M "vicmd" cs change-surround
bindkey -M "vicmd" ds delete-surround
bindkey -M "vicmd" ys add-surround

for m in $visualModes; do
    bindkey -M "$m" S add-surround
done

#  ============================================================================
#                           Autosuggest Config
#  ============================================================================
# This speeds up pasting w/ autosuggest
# https://github.com/zsh-users/zsh-autosuggestions/issues/238
pasteinit() {
    OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
    zle -N self-insert url-quote-magic # I wonder if you'd need `.url-quote-magic`?
}

pastefinish() {
    zle -N self-insert $OLD_SELF_INSERT
}
zstyle :bracketed-paste-magic paste-init pasteinit
zstyle :bracketed-paste-magic paste-finish pastefinish
