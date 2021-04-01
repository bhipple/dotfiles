################################################################################
# Checking for early exit, minimalist shell conditions
simple_terminal() {
    echo "$1"
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='> '
}

if [[ "$TERM" == "dumb" ]]; then
    simple_terminal "Terminal is dumb; using simple cfg"
    return
fi

autoload -U is-at-least
is-at-least 5
if [[ $? -ne 0 ]]; then
    simple_terminal "Zsh < 5.0; using simple cfg"
    return
fi

################################################################################
if [[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ]]; then
    source $HOME/.nix-profile/etc/profile.d/nix.sh
fi

if [[ -f ~/.zsh_local/zshrc_local_before.zsh ]]; then
    source ~/.zsh_local/zshrc_local_before.zsh
fi

fpath+=(~/.zsh/completions $fpath)

source ~/.zsh/plugins.zsh

source ~/.zsh/functions.zsh

source ~/.zsh/settings.zsh

# N.B. fzf should go after settings, for unknown reasons
source ~/.zsh/fzf.zsh

source ~/.zsh/aliases.zsh

if [[ -f ~/.zsh_local/zshrc_local_after.zsh ]]; then
    source ~/.zsh_local/zshrc_local_after.zsh
fi
