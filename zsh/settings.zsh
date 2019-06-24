## ============================================================================
##                                 Settings
## ============================================================================
# Vim mode
bindkey -v

# Command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Visual modes added in zsh 5.0.8
visualModes=()
is-at-least 5.0.8
if [ $? -eq 0 ]; then
    visualModes=("visual")
fi

bindkey -M "vicmd" 'k' history-substring-search-up
bindkey -M "vicmd" 'j' history-substring-search-down

# Run `bindkey -l` to see a list of modes, and `bindkey -M foo` to see a list of
# commands active in mode foo
# Move to vim escape mode
bindkey -M "viins" jj vi-cmd-mode

# Unmap ctrl-s as "stop flow"
stty stop undef

# zsh-autosuggestions cfg
# Bind <CTRL><SPC> to accept and execute
bindkey '^ ' autosuggest-execute

# Shift-tab to cycle backwards in autocompletions
bindkey '^[[Z' reverse-menu-complete

## ============================================================================
##                                  Prompt
## ============================================================================
PROMPT='%{$fg[yellow]%}λ %m %{$fg[green]%}%c%{$fg[yellow]%}$(check_last_exit_code) →  %{$reset_color%}'
RPROMPT=''

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

bindkey -M "vicmd" cs change-surround
bindkey -M "vicmd" ds delete-surround
bindkey -M "vicmd" ys add-surround

for m in $visualModes; do
    bindkey -M "$m" S add-surround
done

#  ============================================================================
#                               FZF Config
#  ============================================================================
if [ -d ~/.fzf/shell ]; then
    source ~/.fzf/shell/completion.zsh
    source ~/.fzf/shell/key-bindings.zsh
fi
