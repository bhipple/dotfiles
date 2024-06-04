# load zgenom
bindkey -v
source "${HOME}/.zgenom/zgenom.zsh"

# Check only when there's no init script
# To regenerate zgenom config from scratch, rm ~/.zgenom/init.zsh
if ! zgenom saved; then
    echo "Creating a zgenom save"

    export DISABLE_AUTO_UPDATE="true"  # Don't ask for oh-my-zsh prompts
    zgenom ohmyzsh
    zgenom ohmyzsh plugins/git-auto-fetch

    # Github plugins
    zgenom load romkatv/powerlevel10k powerlevel10k
    zgenom load romkatv/zsh-prompt-benchmark
    zgenom load djui/alias-tips
    zgenom load zsh-users/zsh-syntax-highlighting

    # save all to init script
    zgenom save

    zgenom compile "$HOME/.zshrc"

    zgenom clean
fi

command -v zoxide 2>&1 >/dev/null && eval "$(zoxide init zsh)"
