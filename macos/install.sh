##
# Move ssh keys before running
##

# Install apps
brew install kitty fish tmux reattach-to-user-namespace emacs gnupg pinentry-mac ag git-delta

# Setup PinEntry GUI
unlink /usr/local/bin/pinentry; ln -s $(which pinentry-mac) /usr/local/bin/pinentry

# Projects
mkdir ~/projects
cd ~/projects

git clone git@github.com:scudelletti/scripts.git


# Setup dotfiles
cd ~/
ln -s ~/projects/scripts/.gitconfig
ln -s ~/projects/scripts/.gitignore.global .gitignore
ln -s ~/projects/scripts/.asdfrc
ln -s ~/projects/scripts/.tmux.conf
ln -s ~/projects/scripts/bin/

# Setup Fish
mkdir -p ~/.config
cd ~/.config
ln -s ~/projects/scripts/linux/config/fish
echo /usr/local/bin/fish | sudo tee -a /etc/shells
chsh -s /usr/local/bin/fish


# Setup GNUPG
mkdir ~/.gnupg && chmod 700 ~/.gnupg && cd ~/.gnupg
ln -s ~/projects/scripts/gpg-agent.conf


# Setup config files
mkdir -p ~/.config
cd ~/.config
ln -s ~/projects/scripts/linux/config/kitty


# Setup Emacs
cd ~/
ln -s ~/projects/scripts/.emacs.d


# Sublime CLI
ln -s "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" /usr/local/bin/subl


# Setup VSCode
cd "/Users/scudelletti/Library/Application Support/Code/User/"
ln -s ~/projects/scripts/vscode/settings.json
ln -s ~/projects/scripts/vscode/keybindings.json
ln -s ~/projects/scripts/vscode/settings.json


# asdf - Check latest version at https://github.com/asdf-vm/asdf
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.0
