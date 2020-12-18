# Enable Autocompletion
autoload -U compinit
compinit

# Load theme
source $HOME/projects/scripts/zsh_theme/scudelletti.zsh

# User configuration
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Fix Tmux Powerline Fonts
export LC_CTYPE=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Export OS type
export OS_TYPE=$(uname | tr '[:upper:]' '[:lower:]')

# Disable Spring
export DISABLE_SPRING=1

# Define default editor
export EDITOR="emacs -nw"

# Enable Erlang Shell History
export ERL_AFLAGS="-kernel shell_history enabled"

# Set Elixir Editor
export ELIXIR_EDITOR="emc +__LINE__ __FILE__"

# Fix Commit Signing
export GPG_TTY=$(tty)

# Aliases
alias tmuxa="tmux a || tmux"
alias got="git"
alias ll="ls -lha"
alias be="bundle exec"
alias bi="bundle check || bundle install"
alias ebashrc="emc ~/.bashrc"
alias ezshrc="emc ~/.zshrc"
alias ehosts="sudo vim /etc/hosts"

if [[ $OS_TYPE = "linux" ]]; then
  alias docker="sudo docker"
  alias docker-compose="sudo docker-compose"
fi

# Folder's aliases
alias projects="cd ~/projects"

# Personal Functions
function some-company() {
  case "$1" in
  'a')
    cd ~/projects/some-company
  ;;
  'f')
    cd ~/projects/some-company/some-folder
  ;;
  'help')
    echo "some-company help Show this help info"
    echo "some-company      Go to some-company folder"
    echo "some-company f    Go to some-company/some-folder folder"
  ;;
  esac
}

function allow-i3() {
  export I3SOCK=$(ls /run/user/1000/i3/ipc-socket.*)
  export DISPLAY=:0
}

# ASDF Scripts
. $HOME/.asdf/asdf.sh

# Add ~/bin to Path
PATH=$PATH:$HOME/bin

# Use GPG Agent as SSH Agent
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

if [[ $OS_TYPE = "linux" ]]; then
  [[ -z "$TMUX" ]] && exec tmux
fi

return 0
