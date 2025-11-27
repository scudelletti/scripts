function tmux_spawn_sessions
  set projects ~/projects/

  for project in $projects
    cd $project && tmuxp devtools
  end

  tmuxa
end
