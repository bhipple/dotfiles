if [[ "$TERM" == "dumb" ]]; then
    echo "Dumb ssh connection; making simple prompt"
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='> '
    return
fi

if [[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ]]; then
    source $HOME/.nix-profile/etc/profile.d/nix.sh
fi

source ~/.zsh/functions.zsh

if [[ -f ~/.zsh_local/zshrc_local_before.zsh ]]; then
    source ~/.zsh_local/zshrc_local_before.zsh
fi

fpath+=(~/.zsh/completions $fpath)

source ~/.zsh/plugins.zsh
source ~/.zsh/settings.zsh
source ~/.zsh/aliases.zsh

if [[ -f ~/.zsh_local/zshrc_local_after.zsh ]]; then
    source ~/.zsh_local/zshrc_local_after.zsh
fi
