# Setup Virtual Console Keymap
sudo mkdir -p /usr/local/share/kdb/keymaps
sudo ln -s ~/projects/scripts/linux/keymap_custom.map /usr/local/share/kdb/keymaps/custom.map

# Setup iptables
sudo ln -s ~/projects/study/ansible-playbooks/templates/iptables/rules.v4 /etc/iptables/iptables.rules
sudo ln -s ~/projects/study/ansible-playbooks/templates/iptables/rules.v6 /etc/iptables/ip6tables.rules
sudo systemctl enable iptables
sudo systemctl start iptables
sudo systemctl enable ip6tables
sudo systemctl start ip6tables

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
