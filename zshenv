skip_global_compinit=1

## ============================================================================
##                           Environment Variables
## ============================================================================
if [ "$TERM" != "screen-256color" ]; then
    export TERM=xterm-256color
fi

export GTEST_COLOR=yes
export EDITOR=vi

cmd=$(uname -a | grep -q "Darwin")
if [ $? -eq 0 ]; then
    export MACOS="true"
    export EDITOR=vim
fi

if hash nvim 2>/dev/null; then
    export EDITOR=nvim
    alias vi=nvim
    alias vim=nvim
fi

# Running in emacs ansi-term
if [ -n "$EMACS" || -n "$INSIDE_EMACS" ]; then
    export AUTO_FU="skip"
fi

# Reduce delay to 0.2 seconds for switching to normal mode with ESC
export KEYTIMEOUT=20

if [ -d /opt/bb/bin ]; then
    PATH=/opt/bb/bin:$PATH
fi

PATH=~/bin_local:$PATH
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

export PYTHONPATH=~/bin

export LIBRARY_PATH="/opt/X11/lib:$LIBRARY_PATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

. ~/bin/resty

[ -f ~/.config/hub ] && export GITHUB_STANDARD_TOKEN=$(grep oauth_token ~/.config/hub | awk '{print $2}')
