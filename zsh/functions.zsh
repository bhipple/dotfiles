# Ask user to confirm command execution
confirm() {
    cmd="$1"
    msg="$2"
    if [ -n "$msg" ]; then
        echo "$msg"
    else
        echo "Would you like to run: $cmd"
    fi
    read -n "resp?Proceed? [y/N] "
    if [[ $resp =~ ^[Yy]$ ]]; then
        eval "$cmd"
    fi
}

# Optionally print a msg, then ask the user if he'd like to continue or abort
proceed() {
    if [ -n "$1" ]; then
        echo "$1"
    fi
    read -n "resp?Would you like to continue? [y/N] "
    if ! [[ $resp =~ ^[Yy]$ ]]; then
        exit 1
    fi
}

tmux_attach() {
    tmux ls || tmux
    session=$(tmux ls | fzf | cut -d':' -f1)
    echo "Connecting to $session"
    tmux a -t $session
}

check_last_exit_code() {
    local LAST_EXIT_CODE=$?
    if [[ $LAST_EXIT_CODE -ne 0 ]]; then
        local EXIT_CODE_PROMPT=' '
        EXIT_CODE_PROMPT+="%{$fg[red]%}-%{$reset_color%}"
        EXIT_CODE_PROMPT+="%{$fg_bold[red]%}$LAST_EXIT_CODE%{$reset_color%}"
        EXIT_CODE_PROMPT+="%{$fg[red]%}-%{$reset_color%} "
        echo "$EXIT_CODE_PROMPT"
    fi
}

set-proxy() {
    export http{,s}_proxy=$1
    export HTTP{,S}_PROXY=$1
    export ftp{,s}_proxy=$1
}

unproxy() {
    unset FTP{,S}_PROXY
    unset ftp{,s}_proxy
    unset HTTP{,S}_PROXY
    unset http{,s}_proxy
    echo "Unset all proxies"
}

vimify() {
    (vim - -esbnN -c "$*" -c 'w!/dev/fd/3|q!' >/dev/null) 3>&1
}

resize() {
    file="$1"
    dimensions="800x800"
    if [ $# -eq 2 ]; then
        dimensions="$2"
    fi

    convert "$file" -resize "$dimensions" "$1"
}

# Get the foo/bar part of https://github.com/foo/bar.git or git@github.com:foo/bar.git
git-remote() {
    REMOTE=${1-origin}
    rmt=$(git remote get-url "$REMOTE" \
              | grep -Eo "([_A-Za-z0-9-]+\/[_A-Za-z0-9-]+(.git)?$)" \
              | sed 's/\.git$//')
    if [[ $? -ne 0 ]]; then
        echo "Failed to get owner/repo from $REMOTE"
        exit 1
    else
        echo "$rmt"
    fi
}

# Get the github.com part of http://github.com/foo/bar.git or git@github.com:foo/bar.git
git-site() {
    REMOTE=${1-origin}
    git remote get-url "$REMOTE" \
              | sed -e 's|^https://||' -e 's|^git@||' -e 's|\.git$||' -e 's|[:/].*||'
}

_git_swapper() {
    repo=$(git-remote "$1")
    git remote set-url "$1" "${2}${repo}.git"
    echo "Set remote $REMOTE to $(git remote get-url $REMOTE)"
}

git-ssh-to-https() {
    site=$(git-site "$1")
    echo "Setting $1 to https on $site"
    _git_swapper "$1" "https://$site/"
}

git-https-to-ssh() {
    site=$(git-site "$1")
    echo "Setting $1 to ssh on $site"
    _git_swapper "$1" "git@$site:"
}

# Swap between HTTPS and SSH, optionally specifying which remote.
git-remote-swap-protocol() {
    REMOTE=${1-origin}
    git remote get-url "$REMOTE" | grep "https" > /dev/null 2>&1
    if [[ $? -eq 0 ]]; then
        git-https-to-ssh "$REMOTE"
    else
        git-ssh-to-https "$REMOTE"
    fi
}
