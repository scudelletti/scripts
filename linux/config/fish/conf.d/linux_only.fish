set -l OS_TYPE (uname | tr '[:upper:]' '[:lower:]')

#if test $OS_TYPE = "linux"
#  [ -z "$TMUX" ] && [ $TERM = "xterm-kitty" ] && exec tmux
#end
