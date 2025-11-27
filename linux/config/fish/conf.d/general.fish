# Set Language
set -gx LANG en_US.UTF-8
set -gx LC_CTYPE en_US.UTF-8

# Define default editor
set -gx EDITOR "emacs -nw"

# Define SHELL Environment Variable
set -gx SHELL (which fish)

# Override TERM to fix colors inside toolboxes
set -gx TERM xterm-direct

# Disable greeting message
set fish_greeting