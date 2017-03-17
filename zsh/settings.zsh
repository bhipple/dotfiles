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

## ============================================================================
##                                  Prompt
## ============================================================================
PROMPT='%{$fg[yellow]%}λ %m %{$fg[green]%}%c%{$fg[yellow]%}$(check_last_exit_code) →  %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=""

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

## ============================================================================
##                              Auto-Fu Config
## ============================================================================
# https://github.com/hchbaw/auto-fu.zsh/issues/29
if [ "$AUTO_FU" = "skip" ]; then
    echo "Skipping auto-fu"
    return
fi

zle-line-init () { auto-fu-init; }; zle -N zle-line-init
zle -N zle-keymap-select auto-fu-zle-keymap-select
zstyle ':completion:*' completer _oldlist _complete
zstyle ':auto-fu:var' postdisplay $'
'

my-reset-prompt-maybe () {
  # XXX: While auto-stuff is in effect,
  # when hitting <Return>, $KEYMAP becomes to `main`:
  # <Return> → `reset-prompt`(*) → `accept-line` → `zle-line-init`
  # → `zle-keymap-select` → `reset-promt` (again!)
  # Skip unwanted `reset-prompt`(*).
  ((auto_fu_init_p==1)) && [[ ${KEYMAP-} == main ]] && return

  # XXX: Please notice that `afu` is treated as Insert-mode-ish.
  RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins|afu)/}"
  zle reset-prompt
}

zle-keymap-select () {
  auto-fu-zle-keymap-select "$@"
  my-reset-prompt-maybe
}
zle -N zle-keymap-select
