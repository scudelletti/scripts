set -l OS_TYPE (uname | tr '[:upper:]' '[:lower:]')

if test $OS_TYPE = "linux"
  # Fix Firefox in wayland
  set -gx MOZ_ENABLE_WAYLAND=1

  [ -z "$TMUX" ] && [ $TERM = "xterm-kitty" ] && exec tmux
end