# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="scudelletti"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM=$HOME/projects/scripts/macos/zsh_custom

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(zsh_reload kubectl)

source $ZSH/oh-my-zsh.sh

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

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

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

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

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

# ASDF Scripts
. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash

# Add ~/bin to Path
PATH=$PATH:$HOME/bin

if [[ $DISPLAY ]]; then
  [[ $- != *i* ]] && return
  [[ -z "$TMUX" ]] && exec tmux
fi
