function tmux_dump_pane -d "Create tmp file with pane content. Use -e to keep escape sequences. $argsv is directly used for tmux capture-pane command."
  set -l TMP_FILE (mktemp -p /tmp/)
  tmux capture-pane -p -S - $argv > $TMP_FILE

  echo file: $TMP_FILE
end