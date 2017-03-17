[[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ]] && \
    source $HOME/.nix-profile/etc/profile.d/nix.sh

source ~/.zsh/functions.zsh

[[ -f ~/.zsh_local/zshrc_local_before.zsh ]] && \
    source ~/.zsh_local/zshrc_local_before.zsh

fpath+=(~/.zsh/completions $fpath)

source ~/.zsh/plugins.zsh
source ~/.zsh/settings.zsh
source ~/.zsh/aliases.zsh

[[ -f ~/.zsh_local/zshrc_local_after.zsh ]] && \
    source ~/.zsh_local/zshrc_local_after.zsh

pathDeduplicate
