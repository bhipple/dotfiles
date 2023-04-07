# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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
fpath+=(\
    ~/.nix-profile/share/zsh/site-functions \
    ~/.zsh/completions \
    $fpath)

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.zsh/p10k.zsh ]] || source ~/.zsh/p10k.zsh

if [[ -f ~/.zsh_local/zshrc_local_before.zsh ]]; then
    source ~/.zsh_local/zshrc_local_before.zsh
fi

source ~/.zsh/plugins.zsh

source ~/.zsh/functions.zsh

source ~/.zsh/settings.zsh

# N.B. fzf should go after settings, for unknown reasons
source ~/.zsh/fzf.zsh

source ~/.zsh/aliases.zsh

if [[ -f ~/.zsh_local/zshrc_local_after.zsh ]]; then
    source ~/.zsh_local/zshrc_local_after.zsh
fi
