# Git Configuration
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="auto"

# Fix Tmux Powerline Fonts
export LC_CTYPE="en_US.UTF-8"

source $HOME/.oh-my-zsh/plugins/gitfast/git-prompt.sh

PROMPT=$'%{\e[33;1;30m%}[%m]%{\e[0m%}  %{$fg[yellow]%}[$(~/.rvm/bin/rvm-prompt)]%{$reset_color%}  %{$fg[red]%}$(__git_ps1 "[%s]")%{$reset_color%}\
%{$fg_bold[blue]%}%n%{$reset_color%} %{\e[33;0;36m%}%~%{\e[0m%} $ '
