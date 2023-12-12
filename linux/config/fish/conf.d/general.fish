# Set Language
set -gx LANG en_US.UTF-8
set -gx LC_CTYPE en_US.UTF-8

# Define default editor
set -gx EDITOR "emacs -nw"

# Define SHELL Environment Variable
set -gx SHELL (which fish)

# Disable greeting message
set fish_greeting