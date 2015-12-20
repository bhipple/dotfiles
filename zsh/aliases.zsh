alias beg='bundle exec guard'
alias g='git'
alias gm='gmake'
alias gvir='gvim --remote-send ":tabe<CR>" && gvim --remote'
alias hd='~/bin/hex_decimal.sh'
alias ll='ls -al'
alias ls='ls --color'
alias mkcd='. ~/bin/make_dir_and_cd.sh'
alias rfwifi='nmcli r wifi off && nmcli r wifi on'
alias sbcl='rlwrap sbcl'
alias scheme='rlwrap scheme'
alias sqlite3='rlwrap sqlite3'

[ -f ~/.zsh_local/aliases_local.zsh ] && source ~/.zsh_local/aliases_local.zsh
