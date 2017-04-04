# load zgen
# auto-fu expects us to be in emacs mode before sourcing (wtf)
bindkey -e
bindkey $'\e' vi-cmd-mode
source "${HOME}/.dotfiles/zgen/zgen.zsh"
bindkey -v

# Check only when there's no init script
# To regenerate zgen config from scratch, rm ~/.zgen/init.zsh
if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh

    # oh-my-zsh plugins
    zgen oh-my-zsh plugins/command-not-found
    zgen oh-my-zsh plugins/wd
    zgen oh-my-zsh plugins/web-search

    # Github plugins
    zgen load djui/alias-tips
    zgen load rupa/z
    zgen load zsh-users/zsh-autosuggestions
    zgen load zsh-users/zsh-completions
    zgen load zsh-users/zsh-history-substring-search
    zgen load zsh-users/zsh-syntax-highlighting
    zgen load StackExchange/blackbox

    # save all to init script
    zgen save
fi
