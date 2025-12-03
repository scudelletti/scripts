#!/bin/sh

while getopts ":ip" opt; do
  case $opt in
    i)
      input_text=$(</dev/stdin)
      copyq copy "$input_text"
      copyq add "$input_text"
      ;;
    p)
      wl-paste --no-newline
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      ;;
  esac
done
