## ============================================================================
##                                 Settings
## ============================================================================
# Command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Hitting ctrl+r for nice history searching
bindkey "^r" history-incremental-search-backward

# Auto-fu adds additional modes to zsh zle. To get keybindings in one of these
# modes with or without auto-fu, bind the key in all modes of that type.
normalModes=("vicmd" "afu-vicmd")
insertModes=("viins" "afu")
if [ "$AUTO_FU" = "skip" ]; then
    normalModes=("vicmd")
    insertModes=("viins")
fi

# Visual modes added in zsh 5.0.8
visualModes=()
is-at-least 5.0.8
if [ $? -eq 0 ]; then
    visualModes=("visual")
fi

for m in $normalModes; do
    bindkey -M vicmd 'k' history-substring-search-up
    bindkey -M vicmd 'j' history-substring-search-down
done

# Run `bindkey -l` to see a list of modes, and `bindkey -M foo` to see a list of commands active in mode foo
# Move to vim escape mode
for m in $insertModes; do
    bindkey -M "$m" jj vi-cmd-mode
    bindkey -M "$m" kk vi-cmd-mode
    bindkey -M "$m" jk vi-cmd-mode
    bindkey -M "$m" kj vi-cmd-mode
done

# Unmap ctrl-s as "stop flow"
stty stop undef

# Vim mode
bindkey -v

# zsh-autosuggestions cfg
# Bind enter to accept and execute
bindkey '^M' autosuggest-execute

## ============================================================================
##                                  Prompt
## ============================================================================
PROMPT='%{$fg[yellow]%}λ %m %{$fg[green]%}%c%{$fg[yellow]%}$(check_last_exit_code) →  %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=""

# Similar to the rename command
# http://www.gsp.com/cgi-bin/man.cgi?section=1&topic=zshcontrib
autoload -U zmv

#  ============================================================================
#                          Text Object Surround
#  ============================================================================
autoload -Uz surround
zle -N delete-surround surround
zle -N add-surround surround
zle -N change-surround surround

for m in $normalModes; do
    bindkey -M "$m" cs change-surround
    bindkey -M "$m" ds delete-surround
    bindkey -M "$m" ys add-surround
done

for m in $visualModes; do
    bindkey -M "$m" S add-surround
done

#  ============================================================================
#                               FZF Config
#  ============================================================================
if ! [ -f ~/.fzf.zsh ]; then
    echo "FZF not installed."
    return
fi

source ~/.fzf.zsh
