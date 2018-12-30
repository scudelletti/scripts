# Setup Virtual Console Keymap
sudo mkdir -p /usr/local/share/kdb/keymaps
sudo ln -s ~/projects/scripts/linux/keymap_custom.map /usr/local/share/kdb/keymaps/custom.map
sudo ln -s ~/projects/scripts/linux/vconsole.conf /etc/vconsole.conf
sudo chown root:root /home/scudelletti/projects/scripts/linux/vconsole.conf

# Setup Keyboard
sudo chmod 644 ~/projects/scripts/linux/xorg.conf/00-keyboard.conf
sudo chown root:root ~/projects/scripts/linux/xorg.conf/00-keyboard.conf
sudo ln -s ~/projects/scripts/linux/xorg.conf/00-keyboard.conf /etc/X11/xorg.conf.d/

# Setup iptables
sudo chown root:root /home/scudelletti/projects/study/ansible-playbooks/templates/iptables/rules.v4
sudo chown root:root /home/scudelletti/projects/study/ansible-playbooks/templates/iptables/rules.v6
sudo ln -s ~/projects/study/ansible-playbooks/templates/iptables/rules.v4  /var/lib/iptables/rules-save
sudo ln -s ~/projects/study/ansible-playbooks/templates/iptables/rules.v6  /var/lib/ip6tables/rules-save
sudo systemctl start ip6tables-restore
sudo systemctl start iptables-restore
sudo systemctl enable ip6tables-restore
sudo systemctl enable iptables-restore

# Enable Magic SysRq key
sudo ln -s ~/projects/scripts/linux/99-sysctl.conf /etc/sysctl.d/99-sysctl.conf

# Setup X
cd
ln -s projects/scripts/linux/.xinitrc
ln -s projects/scripts/linux/.xbindkeysrc
ln -s projects/scripts/linux/.Xresources
ln -s projects/scripts/linux/.xprofile
ln -s projects/scripts/linux/i3 .config/i3
