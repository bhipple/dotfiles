[[ -f ~/.zshenv_local.zsh ]] && . ~/.zshenv_local.zsh

skip_global_compinit=1

## ============================================================================
##                           Environment Variables
## ============================================================================
if ! [[ "$TERM" == "screen-256color" || "$TERM" == "dumb" ]]; then
    export TERM=xterm-256color
fi

export GTEST_COLOR=yes

# Use emacsclient on my desktop/laptop; vim on cloud nodes, etc.
if [[ "$(hostname)" = brh* ]]; then
    export EDITOR=emacsclient
    export VISUAL=emacsclient
else
    export EDITOR=vim
    export VISUAL=vim
fi

cmd=$(uname -a | grep -q "Darwin")
if [ $? -eq 0 ]; then
    export MACOS="true"
fi

if hash nvim 2>/dev/null; then
    alias vi=nvim
    alias vim=nvim
fi

# Running in emacs ansi-term
if [[ -n "$EMACS" || -n "$INSIDE_EMACS" ]]; then
    export AUTO_FU="skip"
fi

# Reduce delay to 0.2 seconds for switching to normal mode with ESC
export KEYTIMEOUT=20

PATH=~/.nix-profile/bin:~/bin_local:$PATH
PATH=~/bin:$PATH
PATH=$PATH:~/.local/bin

if [ -n "$MACOS" ]; then
    PATH=$PATH:~/.stack/programs/x86_64-osx/ghc-7.10.3/bin
    PATH=$PATH:~/Applications/context/tex/texmf-osx-64/bin
else
    PATH=$PATH:~/.stack/programs/x86_64-linux/ghc-7.10.3/bin
fi

PATH=$PATH:~/.cabal/bin
PATH=$PATH:~/.rvm/bin
PATH=$PATH:/opt/chefdk/bin
PATH=$PATH:/bin
PATH=$PATH:/sbin
PATH=$PATH:/usr/local/bin
PATH=$PATH:/usr/bin
PATH=$PATH:/usr/local/sbin
PATH=$PATH:/usr/sbin

export PYTHONPATH=~/bin:$PYTHONPATH

export LIBRARY_PATH="/opt/X11/lib:$LIBRARY_PATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Spacemacs errors if it looks for an env var and it isn't set
export MANPATH=""
export GOPATH=""

export GTAGSLABEL=pygments

. ~/bin/resty

[[ -f ~/.config/hub ]] && export GITHUB_STANDARD_TOKEN=$(grep oauth_token ~/.config/hub | awk '{print $2}')

[[ -f /usr/local/etc/profile.d/nix.sh ]] && . /usr/local/etc/profile.d/nix.sh

export NIX_PATH=src=/home/bhipple/src/nixpkgs:$NIX_PATH
export GPG=gpg2

export FZF_DEFAULT_OPTS='--height 40% --reverse --border'
export FZF_COMPLETION_TRIGGER='~~'

if uname -a | grep -q "NixOS" ; then
    export IS_NIXOS=1
fi

pathDeduplicate() {
    export PATH="$(echo "$PATH" |
        awk 'BEGIN{RS=":";} \
            {sub(sprintf("%c$",10),"");if(A[$0]){}else{A[$0]=1;printf(((NR==1)?"":":")$0)}}' \
        )";
}

pathDeduplicate
