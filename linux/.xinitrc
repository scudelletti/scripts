[[ -f ~/.Xresources ]] && xrdb -merge -I $HOME ~/.Xresources
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
exec i3
