function tmux_spawn_sessions
  set projects_host ~/projects #~/foo ~/bar etc
  set projects_devbox ~/projects/scripts #~/foo ~/bar etc

  for project in $projects_host
    cd $project && tmuxp
  end

  for project in $projects_devbox
    cd $project && tmuxp devtools
  end

  tmuxa
end
