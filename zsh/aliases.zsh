alias battery='upower -i /org/freedesktop/UPower/devices/battery_BAT0'
alias cat='bat --theme=zenburn'
alias cdr.='cd $(git rev-parse --show-toplevel)/..'
alias cdr='cd $(git rev-parse --show-toplevel)'
alias f='z'
alias fn='find . -name'
alias gg='git grep'
alias ggi='git grep -i'
alias hd='~/bin/hex_decimal.sh'
alias jc="sudo journalctl"
alias less='less -N'
alias ll='ls -al'
alias lower="tr '[:upper:]' '[:lower:]'"
alias ls='ls --color'
alias mkcd='. ~/bin/make_dir_and_cd.sh'
alias mu='mupdf-x11'
alias mupdf='mupdf-x11'
alias nb='nix-build'
alias npi='nix path-info -rhsS'
alias rfwifi='nmcli r wifi off && nmcli r wifi on'
alias sc="sudo systemctl"
alias sdiff='diff --side-by-side'
alias sqlite3='rlwrap sqlite3'
alias tn='tmux rename-window $(git rev-parse --show-toplevel | xargs basename)'
alias trashdir='echo "Nuking pwd and all subdirs"; chmod -R a+rwx . && rm -rf *'
alias upper="tr '[:lower:]' '[:upper:]'"
alias v='vagrant'
alias vagrant-purge='killall -9 VBoxHeadless && vagrant destroy'
alias vd='vagrant destroy -f'
alias vr='vagrant destroy -f; vagrant up && vagrant ssh'
alias vs='vagrant ssh'
alias vu='vagrant up'
alias vus='vagrant up && vagrant ssh'
alias waf='./waf -j48'
alias wcb='./waf -v configure build -j48'
alias wfc='curl wttr.in/new_york'
alias wifi='nmcli d wifi'

hash hub > /dev/null 2>&1
if [[ $? -eq 0 ]]; then
    alias git='hub'
    alias g='hub'
fi

# Global Aliases
# Lazily, I'm sourcing my aliases in my bashrc too.
if [[ -n "$ZSH_NAME" ]]; then
    alias -g C='xsel -b'
    alias -g G='rg'
    alias -g GI='rg -i'
    alias -g Gi='rg -i'
    alias -g H='--help'
    alias -g L='less -n'
    alias -g R='!! | less -n'
    alias -g SUM='paste -sd+ - | bc'
fi

# Remove this alias from oh-my-zsh
unalias l

[[ -f ~/.zsh_local/aliases_local.zsh ]] && source ~/.zsh_local/aliases_local.zsh

# Setup autocomplete equivalents
if [ -z "$IN_NIX_SHELL" ]; then
    compdef g='git'
fi

# When processing completions for aliases, expand them to find out what
# function(s) should be completed. Note that this does not work for
# binaries/scripts wrapping the program, such as my bin/g script.
zstyle ':completion:*' completer _expand_alias _complete _ignored
