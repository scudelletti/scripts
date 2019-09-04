# Fix Tmux Powerline Fonts
export LC_CTYPE="en_US.UTF-8"

git_info() {
  # Git branch/tag, or name-rev if on detached head
  local GIT_LOCATION=${$((git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD) 2> /dev/null)#(refs/heads/|tags/)}

  if [ -n "$GIT_LOCATION" ]; then
    local PREFIX="%{$fg[magenta]%}[%{$reset_color%}"
    local SUFIX="%{$fg[magenta]%}]%{$reset_color%}"
    local AHEAD="%{$fg[red]%}⇡NUM%{$reset_color%}"
    local BEHIND="%{$fg[cyan]%}⇣NUM%{$reset_color%}"
    local MERGING="%{$fg_bold[red]%}⚡︎%{$reset_color%}"
    local REBASING="%{$fg[red]%}⚡︎%{$reset_color%}"
    local UNTRACKED="%{$fg[red]%}●%{$reset_color%}"
    local MODIFIED="%{$fg[yellow]%}●%{$reset_color%}"
    local STAGED="%{$fg[green]%}●%{$reset_color%}"
    local STASH="%{$fg_bold[blue]%}¦NUM%{$reset_color%}"

    local -a DIVERGENCES
    local -a FLAGS

    local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
    if [ "$NUM_AHEAD" -gt 0 ]; then
      DIVERGENCES+=( "${AHEAD//NUM/$NUM_AHEAD}" )
    fi

    local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
    if [ "$NUM_BEHIND" -gt 0 ]; then
      DIVERGENCES+=( "${BEHIND//NUM/$NUM_BEHIND}" )
    fi

    local NUM_STASH="$(git stash list --oneline 2> /dev/null | wc -l | tr -d ' ')"
    if [ "$NUM_STASH" -gt 0 ]; then
      DIVERGENCES+=( "${STASH//NUM/$NUM_STASH}" )
    fi

    local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
    if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
      FLAGS+=( "$MERGING" )
    fi

    if [ -n $GIT_DIR ] && (test -r $GIT_DIR/rebase-apply || test -r $GIT_DIR/rebase-merge); then
      FLAGS+=( "$REBASING" )
    fi

    if ! git diff --cached --quiet 2> /dev/null; then
      FLAGS+=( "$STAGED" )
    fi

    if ! git diff --quiet 2> /dev/null; then
      FLAGS+=( "$MODIFIED" )
    fi

    if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
      FLAGS+=( "$UNTRACKED" )
    fi

    local -a GIT_INFO
    GIT_INFO+=( $PREFIX )
    [ -n "$GIT_STATUS" ] && GIT_INFO+=( "$GIT_STATUS" )
    GIT_INFO+=( "$fg[magenta]$GIT_LOCATION%{$reset_color%} " )
    [[ ${#DIVERGENCES[@]} -ne 0 ]] && GIT_INFO+=( "${(j::)DIVERGENCES}" )
    [[ ${#FLAGS[@]} -ne 0 ]] && GIT_INFO+=( "${(j::)FLAGS}" )
    GIT_INFO+=( $SUFIX )
    print "${(j::)GIT_INFO}"
  fi
}


# We need to store the last exit status and return it on the end to avoid changing it
__personal_hostname_ps1 () {
  local exit=$?

  print "%{\e[2;37;40m%}[%m]%{\e[0m%}"

  return $exit
}

__personal_last_exit_code_ps1() {
  local exit=$?

  if [ "$exit" = "0" ]; then
    print "%{$fg[green]%}[$exit]%{$reset_color%}"
  else
    print "%{$fg[red]%}[$exit]%{$reset_color%}"
  fi

  return $exit
}

__personal_ruby_version_ps1 () {
  local exit=$?

  local ruby_version=$(ruby -v | cut -f2 -d ' ' | cut -f1 -d 'p')

  print "%{$fg[yellow]%}[$ruby_version]%{$reset_color%}"

  return $exit
}

__personal_git_ps1() {
  local exit=$?

  print $(git_info)

  return $exit
}

__personal_username_ps1 () {
  local exit=$?

  print "%{$fg_bold[blue]%}%n%{$reset_color%}"

  return $exit
}

__personal_path_ps1 () {
  local exit=$?

  print "%{\e[33;0;36m%}%~%{\e[0m%}"

  return $exit
}

PROMPT=$'$(__personal_hostname_ps1)  $(__personal_last_exit_code_ps1)  $(__personal_ruby_version_ps1)  $(__personal_git_ps1)\
$(__personal_username_ps1) $(__personal_path_ps1) $ '
