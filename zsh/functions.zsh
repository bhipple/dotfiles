# Ask user to confirm command execution
confirm() {
    cmd="$1"
    msg="$2"
    echo "Evaluating: $cmd"
    if [ -n "$msg" ]; then
        echo "$msg"
    fi
    read -n "resp?Are you sure? [y/N] "
    if [[ $resp =~ ^[Yy]$ ]]; then
        eval "$cmd"
    fi
}

vimdf() {
    nvim -p \
        $(git diff --name-only --diff-filter=M | tr '\n' ' ')
}

em() {
    if [ "$#" -eq 0 ]; then
        emacs ~/org/me.org
    elif [ "$1" = "e" ]; then
        emacs ~/.emacs
    else
        emacs "$@"
    fi
}

pathDeduplicate() {
    export PATH="$(echo "$PATH" |
        awk 'BEGIN{RS=":";} \
            {sub(sprintf("%c$",10),"");if(A[$0]){}else{A[$0]=1;printf(((NR==1)?"":":")$0)}}' \
        )";
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

cd() {
    builtin cd "$@" || return
    rename-tmux.sh >/dev/null 2>&1
    return 0
}

noproxy() {
    env -u http_proxy -u https_proxy -u HTTP_PROXY -u HTTPS_PROXY "$@"
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

#  ============================================================================
#                                   FZF
#  ============================================================================
#  Useful functions from https://github.com/junegunn/fzf/wiki/examples
#
# fh - repeat history. This function only works in zsh
fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}

# fbr - checkout git branch
fbr() {
  local branches branch
  branches=$(git branch -vv) &&
  branch=$(echo "$branches" | fzf +m) &&
  git checkout $(echo "$branch" | awk '{print $1}' | sed "s/.* //")
}

# fbr - checkout git branch (including remote branches)
fbr() {
  local branches branch
  branches=$(git branch --all | grep -v HEAD) &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

# fco - checkout git branch/tag
fco() {
  local tags branches target
  tags=$(
    git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
  branches=$(
    git branch --all | grep -v HEAD             |
    sed "s/.* //"    | sed "s#remotes/[^/]*/##" |
    sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
  target=$(
    (echo "$tags"; echo "$branches") |
    fzf-tmux -l30 -- --no-hscroll --ansi +m -d "\t" -n 2) || return
  git checkout $(echo "$target" | awk '{print $2}')
}

# fcoc - checkout git commit
fcoc() {
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}

# fshow - git commit browser
fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}
# fcs - get git commit sha
# example usage: git rebase -i `fcs`
fcs() {
  local commits commit
  commits=$(git log --color=always --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e --ansi --reverse) &&
  echo -n $(echo "$commit" | sed "s/ .*//")
}

# fstash - easier way to deal with stashes
# type fstash to get a list of your stashes
# enter shows you the contents of the stash
# ctrl-d shows a diff of the stash against your current HEAD
# ctrl-b checks the stash out as a branch, for easier merging
fstash() {
  local out q k sha
  while out=$(
    git stash list --pretty="%C(yellow)%h %>(14)%Cgreen%cr %C(blue)%gs" |
    fzf --ansi --no-sort --query="$q" --print-query \
        --expect=ctrl-d,ctrl-b);
  do
    mapfile -t out <<< "$out"
    q="${out[0]}"
    k="${out[1]}"
    sha="${out[-1]}"
    sha="${sha%% *}"
    [[ -z "$sha" ]] && continue
    if [[ "$k" == 'ctrl-d' ]]; then
      git diff $sha
    elif [[ "$k" == 'ctrl-b' ]]; then
      git stash branch "stash-$sha" $sha
      break;
    else
      git stash show -p $sha
    fi
  done
}

#  ============================================================================
#                                Stacksort
#  ============================================================================
# Who needs copying and pasting from Stack Overflow when we can do it on the cmdline!
stacksort() {
    echo $@ | ( (vim - -esbnN -c '.s/\(.*\)/\=system('"'"'a='"'"'."https:\/\/api.stackexchange.com\/2.2\/".'"'"'; q=`curl -s -G --data-urlencode "q='"'"'.submatch(1).'"'"'" --compressed "'"'"'."${a}search\/advanced?order=desc&sort=relevance&site=stackoverflow".'"'"'" | python -c "'"'"'."exec(\\\"import sys \\nimport json\\nprint(json.loads('"'"''"'"'.join(sys.stdin.readlines()))['"'"'items'"'"'][0]['"'"'question_id'"'"'])\\\")".'"'"'"`; curl -s --compressed "'"'"'."${a}questions\/$q\/answers?order=desc&sort=votes&site=stackoverflow&filter=withbody".'"'"'" | python -c "'"'"'."exec(\\\"import sys\\nimport json\\nprint(json.loads('"'"''"'"'.join(sys.stdin.readlines()))['"'"'items'"'"'][0]['"'"'body'"'"']).encode('"'"'utf8'"'"')\\\")".'"'"'"'"'"')/' -c '%s/\(\_.\+\)<code>\(\_.\+\)<\/code>\(\_.\+\)/\2/g' -c '%s/&gt;/>/g' -c 'w!/dev/stderr|q!' >/dev/null) 2>&1)
}

stacksort-raw() {
    echo $@ | ( (vim - -esbnN -c '.s/\(.*\)/\=system('"'"'a='"'"'."https:\/\/api.stackexchange.com\/2.2\/".'"'"'; q=`curl -s -G --data-urlencode "q='"'"'.submatch(1).'"'"'" --compressed "'"'"'."${a}search\/advanced?order=desc&sort=relevance&site=stackoverflow".'"'"'" | python -c "'"'"'."exec(\\\"import sys \\nimport json\\nprint(json.loads('"'"''"'"'.join(sys.stdin.readlines()))['"'"'items'"'"'][0]['"'"'question_id'"'"'])\\\")".'"'"'"`; curl -s --compressed "'"'"'."${a}questions\/$q\/answers?order=desc&sort=votes&site=stackoverflow&filter=withbody".'"'"'" | python -c "'"'"'."exec(\\\"import sys\\nimport json\\nprint(json.loads('"'"''"'"'.join(sys.stdin.readlines()))['"'"'items'"'"'][0]['"'"'body'"'"']).encode('"'"'utf8'"'"')\\\")".'"'"'"'"'"')/' -c 'w!/dev/stderr|q!' >/dev/null) 2>&1)
}
