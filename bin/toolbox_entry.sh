#!/bin/sh

read -p "TOOLBOX: " PTOOLBOX_TYPE
read -p "CONTAINER: " PTOOLBOX_CONTAINER_NAME

#PTOOLBOX_TYPE="toolbox"
#PTOOLBOX_CONTAINER_NAME=""

DEFAULT_COMMAND=""
case "$PTOOLBOX_TYPE" in
   "") DEFAULT_COMMAND=""
   ;;
   "toolbox") DEFAULT_COMMAND="toolbox enter $PTOOLBOX_CONTAINER_NAME"
   ;;
   "distrobox") DEFAULT_COMMAND="distrobox enter $PTOOLBOX_CONTAINER_NAME"
   ;;
   "t") DEFAULT_COMMAND="toolbox enter $PTOOLBOX_CONTAINER_NAME"
   ;;
   "d") DEFAULT_COMMAND="distrobox enter $PTOOLBOX_CONTAINER_NAME"
   ;;
   *) DEFAULT_COMMAND="";
   ;;
esac

# Use tmux from host machine when inside a toolbox container
COMMAND_PREFIX=""
if [ -n "$CONTAINER_ID" ]; then
  COMMAND_PREFIX="distrobox-host-exec"
fi

$COMMAND_PREFIX tmux set-option default-command "$DEFAULT_COMMAND"
