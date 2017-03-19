#!/usr/bin/env bash
# shellcheck disable=SC1090

## don't read bashrc if session is not interactive
[ -z "$PS1" ] && return
echo "Loading bashrc"

## ============================================================================
##                                  General
## ============================================================================
# Set history
set -o history
set history on 10000

# update winsize after each command for better line-wrapping
shopt -s checkwinsize

# Source my generic aliases
[ -f ~/.bash_aliases ] && . ~/.bash_aliases

[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ] && \
    . $HOME/.nix-profile/etc/profile.d/nix.sh

# Prompt
PS1='\[\e[1;31m\][\u@\h: \w]\$\[\e[0m\] '

# Pull data out of the zsh files
if [ -f ~/.zsh_local/zshrc_local_before.zsh ]; then
    source ~/.zsh_local/zshrc_local_before.zsh
fi

source ~/.zsh/aliases.zsh

if [ -f ~/.zsh_local/zshrc_local_after.zsh ]; then
    source ~/.zsh_local/zshrc_local_after.zsh
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
