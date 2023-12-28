# load zgenom
bindkey -v
source "${HOME}/.zgenom/zgenom.zsh"

# Check only when there's no init script
# To regenerate zgenom config from scratch, rm ~/.zgenom/init.zsh
if ! zgenom saved; then
    echo "Creating a zgenom save"

    export DISABLE_AUTO_UPDATE="true"  # Don't ask for oh-my-zsh prompts
    zgenom oh-my-zsh

    zgenom load romkatv/powerlevel10k powerlevel10k

    # Github plugins
    zgenom load djui/alias-tips
    zgenom load romkatv/zsh-prompt-benchmark
    zgenom load zsh-users/zsh-syntax-highlighting

    # save all to init script
    zgenom save

    zgenom compile "$HOME/.zshrc"

    zgenom clean
fi

eval "$(zoxide init zsh)"
