# load zgen
bindkey -v
source "${HOME}/dotfiles/zgen/zgen.zsh"

eval "$(zoxide init zsh)"

# Check only when there's no init script
# To regenerate zgen config from scratch, rm ~/.zgen/init.zsh
if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh

    # Github plugins
    zgen load djui/alias-tips
    zgen load zsh-users/zsh-autosuggestions
    zgen load zsh-users/zsh-completions
    zgen load zsh-users/zsh-syntax-highlighting

    # save all to init script
    zgen save
fi
