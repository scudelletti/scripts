# Git Configuration
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="auto"

# Fix Tmux Powerline Fonts
export LC_CTYPE="en_US.UTF-8"

source $HOME/.oh-my-zsh/plugins/gitfast/git-prompt.sh

# We need to store the last exit status and return it on the end to avoid changing it
__personal_hostname_ps1 () {
  local exit=$?

  print "%{\e[33;1;30m%}[%m]%{\e[0m%}"

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
  
  branch_name=$(__git_ps1 "[%s]" || "")

  if [[ $branch_name =~ "^\[master.{0,5}\]$" ]]; then
    print "%{\e[47m$fg[red]%}$branch_name%{$reset_color%}"
  else
    print "%{$fg[magenta]%}$branch_name%{$reset_color%}"
  fi

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
