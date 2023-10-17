#!/bin/sh

DEFAULT_COMMAND=""

read -p "TOOLBOX: " PTOOLBOX_TYPE
read -p "CONTAINER: " PTOOLBOX_CONTAINER_NAME

#PTOOLBOX_TYPE="toolbox"
#PTOOLBOX_CONTAINER_NAME=""

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

tmux set-option default-command "$DEFAULT_COMMAND"
