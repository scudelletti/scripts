# Set Language
set -gx LANG en_US.UTF-8
set -gx LC_CTYPE en_US.UTF-8

# Define default editor
set -gx EDITOR "emacs -nw"

# Define SHELL Environment Variable
set -gx SHELL (which fish)

# Override TERM to fix colors inside toolboxes
if test -n "$CONTAINER_ID"
  set -gx TERM xterm-direct
end

# Set DOCKER_HOST for docker-compose
set -gx DOCKER_HOST "unix:///run/user/1000/podman/podman.sock"

# Disable greeting message
set fish_greeting