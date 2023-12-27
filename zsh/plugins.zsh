# load zgen
bindkey -v
source "${HOME}/dotfiles/zgen/zgen.zsh"

# Check only when there's no init script
# To regenerate zgen config from scratch, rm ~/.zgen/init.zsh
if ! zgen saved; then
    echo "Creating a zgen save"

    export DISABLE_AUTO_UPDATE="true"  # Don't ask for oh-my-zsh prompts
    zgen oh-my-zsh

    zgen load romkatv/powerlevel10k powerlevel10k

    # Github plugins
    zgen load djui/alias-tips
    zgen load romkatv/zsh-prompt-benchmark
    zgen load zsh-users/zsh-syntax-highlighting

    # save all to init script
    zgen save
fi

eval "$(zoxide init zsh)"
