#!/bin/sh

while getopts ":ip" opt; do
  case $opt in
    i)
      input_text=$(</dev/stdin)
      copyq copy "$input_text"
      copyq add "$input_text"
      ;;
    p)
      copyq read
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      ;;
  esac
done
