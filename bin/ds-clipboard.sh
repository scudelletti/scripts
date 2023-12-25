#!/bin/sh

while getopts ":ip" opt; do
  case $opt in
    i)
      read input_text
      flatpak run com.github.hluk.copyq copy "$input_text"
      flatpak run com.github.hluk.copyq add "$input_text"
      ;;
    p)
      wl-paste --no-newline
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      ;;
  esac
done
