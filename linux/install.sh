# Setup Virtual Console Keymap
sudo mkdir -p /usr/local/share/kdb/keymaps
sudo ln -s ~/projects/scripts/linux/keymap_custom.map /usr/local/share/kdb/keymaps/custom.map

# Enable Magic SysRq key
sudo ln -s ~/projects/scripts/linux/99-sysctl.conf /etc/sysctl.d/99-sysctl.conf

# Setup X
cd
ln -s projects/scripts/linux/.xinitrc
ln -s projects/scripts/linux/.Xmodmap
ln -s projects/scripts/linux/.xbindkeysrc
ln -s projects/scripts/linux/.Xresources
ln -s projects/scripts/linux/.xprofile
ln -s projects/scripts/linux/i3 .config/i3
