# Setup Virtual Console Keymap
sudo mkdir -p /usr/local/share/kdb/keymaps
sudo ln -s ~/projects/scripts/linux/keymap_custom.map /usr/local/share/kdb/keymaps/custom.map

# Setup X
ln -s projects/scripts/linux/.xinitrc
ln -s projects/scripts/linux/.Xmodmap
ln -s projects/scripts/linux/.Xresources
ln -s projects/scripts/linux/i3 .config/i3
