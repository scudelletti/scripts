# Enable Autocompletion
autoload -U compinit
compinit

# Navigate words like bash when using word navigation such as meta+b
autoload -U select-word-style
select-word-style bash

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
alias ll="ls -lha"
alias be="bundle exec"
alias bi="bundle check || bundle install"

if [[ $OS_TYPE = "linux" ]]; then
  alias docker="sudo docker"
  alias docker-compose="sudo docker-compose"
fi

# Create new tmux session with directory as the session's name
function tmuxp () {
  local SESSION_NAME=$(basename "$PWD")

  # Create new session
  tmux new-session -s $SESSION_NAME -d -c $PWD

  # Set environment variable for the new session
  tmux set-environment -t $SESSION_NAME EMACS_SOCKET $SESSION_NAME

  # Recreate the panel so we can have access to the new environment variable
  # This is necessary since a pane was created before the environment variable was set
  tmux split-window -t $SESSION_NAME:0 && tmux kill-pane -t $SESSION_NAME:0.0
}

function port_listening() {
  lsof -i :$1 | grep 'LISTEN'
}

# Copy Branch Name
function cbn() {
  echo -n $(git symbolic-ref --short -q HEAD) | pbcopy
}

# ASDF Scripts
. $HOME/.asdf/asdf.sh

# Add ~/bin to Path
PATH=$PATH:$HOME/bin

# Add ~/bin/transient to Path
PATH=$PATH:$HOME/bin/transient

# Use GPG Agent as SSH Agent
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

if [[ $OS_TYPE = "linux" ]]; then
  # Fix Firefox in wayland
  export MOZ_ENABLE_WAYLAND=1

  [ -z "$TMUX" ] && [ $TERM = "xterm-kitty" ] && exec tmux
fi

return 0
