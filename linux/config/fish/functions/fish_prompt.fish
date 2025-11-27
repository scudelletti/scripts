function fish_prompt
    set -g ds_status $status

    ##
    # Git Info
    ##
    set -g __fish_git_prompt_show_informative_status 1
    set -g __fish_git_prompt_showdirtystate 1
    set -g __fish_git_prompt_showuntrackedfiles 1
    set -g __fish_git_prompt_showupstream informative
    set -g __fish_git_prompt_showcolorhints 1
    set -g __fish_git_prompt_use_informative_chars 1
    set -g __fish_git_prompt_showstashstate 1

    set -g __fish_git_prompt_color_branch purple
    set -g __fish_git_prompt_color_branch_detached "red"

    set -g __fish_git_prompt_char_cleanstate ""
    set -g __fish_git_prompt_char_stateseparator "¦"
    set -g __fish_git_prompt_color_stashstate "blue"
    set -g __fish_git_prompt_color_dirtystate "yellow"
    set -g __fish_git_prompt_color_untrackedfiles "red"

    set -l vcs (fish_vcs_prompt "[%s]" 2>/dev/null)

    ##
    # Path
    ##
    # Disable PWD shortening by default
    set -q fish_prompt_pwd_dir_length
    or set -lx fish_prompt_pwd_dir_length 0

    set -l path (set_color cyan; prompt_pwd; set_color normal)

    ##
    # Hostname with Toolbox Container
    ##
    function ds_hostname_with_toolbox_fn
      set -l machine_name (set_color brblack; printf (prompt_hostname); set_color normal)
      set -l toolbox_name (set_color brblue; printf $CONTAINER_ID; set_color normal)

      if test -n "$CONTAINER_ID"
         printf "[%s|%s]" "$toolbox_name" $machine_name
      else
         printf "[%s]" "$machine_name"
      end
    end

    ##
    # Status
    ##
    function ds_status_fn
        if test "$ds_status" = 0
            set_color green
        else
            set_color red
        end

        echo [$ds_status]
        set_color normal
        set -e ds_status
    end

    ##
    # User
    ##
    set -l user (set_color blue; echo $USER; set_color normal)

    ##
    # Prompt
    ##
    string join " " -- (ds_hostname_with_toolbox_fn) (ds_status_fn) $vcs
    string join " " -- $user $path '$ '
end
