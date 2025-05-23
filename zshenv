# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

skip_global_compinit=1

# Don't nice background processes
unsetopt BG_NICE

## ============================================================================
##                           Environment Variables
## ============================================================================
if [[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ]]; then
    source $HOME/.nix-profile/etc/profile.d/nix.sh
fi

export GTEST_COLOR=yes

export EDITOR=nvim
export VISUAL=nvim

if [[ -e ~/.terminfo/x/xterm-24bit ]]; then
    export COLORTERM=truecolor
fi

# Delay to wait in hundredths of seconds for switching to normal mode with ESC
export KEYTIMEOUT=0

export PATH=\
~/bin_local\
:~/bin\
:~/git/nixpkgs/result/bin\
:~/.nix-profile/bin\
:~/.local/bin\
:/run/wrappers/bin\
:/etc/profiles/per-user/$USER/bin\
:/nix/var/nix/profiles/$USER/bin\
:/run/current-system/sw/bin\
:/usr/local/bin\
:/usr/bin\

# Enable full support for Unicode explicitly
export LANG=en_US.UTF-8

# Spacemacs errors if it looks for an env var and it isn't set
export MANPATH=""
export GOPATH=""

export GTAGSLABEL=pygments

export GPG=gpg2

export FZF_DEFAULT_OPTS='--height 40% --reverse --border'
export FZF_COMPLETION_TRIGGER='~~'

if uname -a | grep -q "NixOS"; then
    export IS_NIXOS=1
fi

# Never ask to auto-update oh-my-zsh; instead require `zgenom update`
export DISABLE_AUTO_UPDATE=true

# Source local config, if any
[[ -f ~/.zshenv_local.zsh ]] && . ~/.zshenv_local.zsh
