# RVM Configuration
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
source ~/.rvm/scripts/rvm

# Git Configuration
# enable the git bash completion commands
source /usr/local/etc/bash_completion.d/git-completion.bash
# enable the git functions to PS1
source /usr/local/etc/bash_completion.d/git-prompt.sh

# Ambient Variables to git PS1
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="auto"

# Setting the color on variables
NO_COLOUR="\[\033[0m\]"
BLACK="\[\033[0;30m\]"
DARK_GRAY="\[\033[0;30m\]"
LIGHT_GRAY="\[\033[1;30m\]"
BLUE="\[\033[0;34m\]"
LIGHT_BLUE="\[\033[1;34m\]"
GREEN="\[\033[0;32m\]"
CYAN="\[\033[0;36m\]"
RED="\[\033[0;31m\]"
PURPLE="\[\033[0;35m\]"
BROWN="\[\033[0;33m\]"
YELLOW="\[\033[0;33m\]"

# RVM Function to show the version used in PS1
RVM="\$(~/.rvm/bin/rvm-prompt)"

# PS1 Configuration with GIT, RVM and AWS Env
PS1="$LIGHT_GRAY[\h] $NO_COLOUR $YELLOW[$RVM]$PURPLE\$(__aws_env) $RED\$(__git_ps1 ' [%s]')\n$LIGHT_BLUE\u $CYAN\w$NO_COLOUR \$ "

# Tmux save the path each time the shell prompt is displayed - For Tmux-Powerline
PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'

# Aliases
alias got="git"
alias ll="ls -la"
alias be="bundle exec"
alias ebashrc="sublime ~/.bashrc"
#alias run-spec="cd ~/projects/some_project/ && pwd && echo Running All Tests on Some Project && rake spec:all"

# Folder's aliases
alias projects="cd ~/projects"

# Amazon Variables
# Dev
DEV_AWS_ACCESS_KEY_ID=AAAA
DEV_AWS_SECRET_ACCESS_KEY=BBBB

# Production
PROD_AWS_ACCESS_KEY_ID=CCCC
PROD_AWS_SECRET_ACCESS_KEY=DDDD

# Function to set AWS ambient variables
function aws_for() {
  if [[ $1 = 'prod' ]]; then
    echo 'Exporting AWS variables for Prod environment'
    export AWS_ENV='prod'
    export AWS_ACCESS_KEY_ID=$PROD_AWS_ACCESS_KEY_ID
    export AWS_SECRET_ACCESS_KEY=$PROD_AWS_SECRET_ACCESS_KEY
  else
    echo 'Exporting AWS variables for Dev environment'
    export AWS_ENV='dev'
    export AWS_ACCESS_KEY_ID=$DEV_AWS_ACCESS_KEY_ID
    export AWS_SECRET_ACCESS_KEY=$DEV_AWS_SECRET_ACCESS_KEY
  fi
}

# Function used on PS1
__aws_env() {
  if [ $AWS_ENV ]; then
    echo "  [aws:$AWS_ENV]"
  else
    echo ""
  fi
}

# Add /usr/local/sbin to $PATH
PATH=/usr/local/sbin:$PATH

# Put all brew binaries on $PATH
for i in $(find /usr/local/Cellar -name bin)
do
  PATH=$i:$PATH
done

# Add Bash Completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi
