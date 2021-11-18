function tmuxp
  set -l SESSION_NAME (basename "$PWD")

  # Create new session
  tmux new-session -s $SESSION_NAME -d -c $PWD

  # Set environment variable for the new session
  tmux set-environment -t $SESSION_NAME EMACS_SOCKET $SESSION_NAME

  # Recreate the panel so we can have access to the new environment variable
  # This is necessary since a pane was created before the environment variable was set
  tmux split-window -t $SESSION_NAME:0 && tmux kill-pane -t $SESSION_NAME:0.0
end
