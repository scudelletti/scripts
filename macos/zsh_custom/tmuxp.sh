function tmuxp() {
  local SESSION_NAME=$(basename "$PWD")

  tmux new-session -s $SESSION_NAME -d -c $PWD && \
  tmux set-environment -t $SESSION_NAME EMACS_SOCKET $SESSION_NAME && \
  tmux split-window -t $SESSION_NAME:0 && \
  tmux kill-pane -t $SESSION_NAME:0.0
}
