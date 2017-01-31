#!/usr/bin/env bash
# shellcheck disable=SC1090

## don't read bashrc if session is not interactive
[ -z "$PS1" ] && return
echo "Loading bashrc"

## ============================================================================
##                                  General
## ============================================================================
export TERM=screen-256color
export EDITOR=vi

# Set history
set -o history
export histexpand=on
set history on 10000

# update winsize after each command for better line-wrapping
shopt -s checkwinsize

# Source my generic aliases
[ -f ~/.bash_aliases ] && . ~/.bash_aliases

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
