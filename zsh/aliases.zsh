alias battery='upower -i /org/freedesktop/UPower/devices/battery_BAT0'
alias cdr.='cd $(git rev-parse --show-toplevel)/..'
alias cdr='cd $(git rev-parse --show-toplevel)'
alias ff='firefox'
alias fn='find . -name'
alias g='git'
alias gg='git grep'
alias ggi='git grep -i'
alias gm='gmake -j'
alias gmc='gmake -j clean'
alias gmr='gmake clean -j && gmake -j'
alias gmt='gmake test -j'
alias gvir='gvim --remote-send ":tabe<CR>" && gvim --remote'
alias hd='~/bin/hex_decimal.sh'
alias hdevtools='stack exec --no-ghc-package-path hdevtools --'
alias jc="sudo journalctl"
alias l='ledger'
alias less='less -N'
alias ll='ls -al'
alias lower="tr '[:upper:]' '[:lower:]'"
alias ls='ls --color'
alias m='make -j'
alias mc='make -j clean'
alias mgt='make -j gtest'
alias mkcd='. ~/bin/make_dir_and_cd.sh'
alias mt='make -j test'
alias mu='mupdf-x11'
alias mupdf='mupdf-x11'
alias myip='curl ipinfo.io'
alias nix-deps='nix path-info -r'
alias rfwifi='nmcli r wifi off && nmcli r wifi on'
alias s='stack'
alias sb='stack build --file-watch'
alias sbcl='rlwrap sbcl'
alias sbe='stack bench --file-watch'
alias sc="sudo systemctl"
alias scheme='rlwrap scheme'
alias sdiff='diff --side-by-side'
alias se='stack exec'
alias sho='stack haddock --open --file-watch'
alias si='stack install'
alias sqlite3='rlwrap sqlite3'
alias st='stack test --file-watch'
alias tn='tmux rename-window $(git rev-parse --show-toplevel | xargs basename)'
alias upper="tr '[:lower:]' '[:upper:]'"
alias uu='sudo apt-get update && sudo apt-get upgrade'
alias v='vagrant'
alias vagrant-purge='killall -9 VBoxHeadless && vagrant destroy'
alias vd='vagrant destroy -f'
alias vr='vagrant destroy -f; vagrant up && vagrant ssh'
alias vs='vagrant ssh'
alias vu='vagrant up'
alias vus='vagrant up && vagrant ssh'
alias waf='./waf'
alias wcb='./waf -v configure build'
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
    alias -g G='grep'
    alias -g GI='grep -i'
    alias -g Gi='grep -i'
    alias -g H='--help'
    alias -g L='less -n'
    alias -g R='!! | less -n'
    alias -g SUM='paste -sd+ - | bc'
fi

[[ -f ~/.zsh_local/aliases_local.zsh ]] && source ~/.zsh_local/aliases_local.zsh

# Setup autocomplete equivalents
if [ -z "$IN_NIX_SHELL" ]; then
    compdef g='git'
fi
