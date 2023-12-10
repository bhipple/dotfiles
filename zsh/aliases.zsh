alias battery='upower -i /org/freedesktop/UPower/devices/battery_BAT0'
alias cat='bat --theme=zenburn'
alias cdr.='cd $(git rev-parse --show-toplevel)/..'
alias cdr='cd $(git rev-parse --show-toplevel)'
alias f='zi'
# TODO: can probably codegen these with `git la` and some grepping?
alias gbr='git br'
alias gco='git co'
alias gdf='git df'
alias gg='git grep'
alias ggi='git grep -i'
alias gl='git l'
alias gst='git st'
alias hd='~/bin/hex_decimal.sh'
alias jc="sudo journalctl"
alias less='less -N'
alias ll='ls -al'
alias lower="tr '[:upper:]' '[:lower:]'"
alias ls='ls --color'
alias mu='mupdf-x11'
alias nb='nix build -Lf .'
alias nbc='nix build -Lf $HOME/git/nix-channel'
alias npi='nix path-info -rhsS'
alias rfwifi='nmcli r wifi off && nmcli r wifi on'
alias sc="sudo systemctl"
alias sdiff='diff --side-by-side'
alias sqlite3='rlwrap sqlite3'
alias ta='tmux_attach'
alias tn='tmux rename-window $(git rev-parse --show-toplevel | xargs basename)'
alias trashdir='echo "Nuking pwd and all subdirs"; chmod -R a+rwx . && rm -rf *'
alias upper="tr '[:lower:]' '[:upper:]'"
alias wfc='curl wttr.in/new_york'
alias wifi='nmcli d wifi'

if hash nvim > /dev/null 2>&1; then
    vim_cmd='XTERM_VERSION= nvim -u ~/.config/nvim/init.lua'
else
    vim_cmd='vim'
fi
alias e="$vim_cmd"
alias v="$vim_cmd"
alias nvim="$vim_cmd"

# Global Aliases
# Lazily, I'm sourcing my aliases in my bashrc too.
if [[ -n "$ZSH_NAME" ]]; then
    alias -g C='| xsel -b'
    alias -g F='| fzf'
    alias -g G='| rg'
    alias -g GI='| rg -i'
    alias -g GV='| rg -v'
    alias -g Gi='| rg -i'
    alias -g Gv='| rg -v'
    alias -g H='| head'
    alias -g K='--keep-going'
    alias -g L='| less -n'
    alias -g NV='--no-verify'
    alias -g PS='ps aux'
    alias -g R='!! | less -n'
    alias -g S='| sort'
    alias -g SUM='| sed "s|[,$]||g" | paste -sd+ - | bc'
    alias -g T='| tail'
    alias -g W='| wc -l'
    alias -g X='| xargs -I{}'
    alias -g Y='--yes'

    alias -g P1="2>&1 | awk '{print \$1}'"
    alias -g P2="2>&1 | awk '{print \$2}'"
    alias -g P3="2>&1 | awk '{print \$3}'"
    alias -g P4="2>&1 | awk '{print \$4}'"
    alias -g P5="2>&1 | awk '{print \$5}'"
    alias -g P6="2>&1 | awk '{print \$6}'"
    alias -g P7="2>&1 | awk '{print \$7}'"
    alias -g P8="2>&1 | awk '{print \$8}'"
    alias -g P9="2>&1 | awk '{print \$9}'"
    alias -g PN="2>&1 | awk '{print \$NF}'"
    alias -g PNF="2>&1 | awk '{print \$NF}'"
fi

[[ -f ~/.zsh_local/aliases_local.zsh ]] && source ~/.zsh_local/aliases_local.zsh

if whence -w l | grep -q alias; then
    unalias l
fi

# Setup autocomplete equivalents
if [ -z "$IN_NIX_SHELL" ]; then
    compdef g='git'
fi

# Only do this at home
if [[ $USER == "bhipple" ]]; then
    compdef p='pass'
fi

# When processing completions for aliases, expand them to find out what
# function(s) should be completed. Note that this does not work for
# binaries/scripts wrapping the program, such as my bin/g script.
zstyle ':completion:*' completer _expand_alias _complete _ignored
