##
# Move ssh keys before running
##

# Install Kitty
brew cask install kitty


# Install apps
brew install tmux reattach-to-user-namespace emacs gnupg pinentry-mac


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


# Setup ZSH
rm .zshrc
ln -s ~/projects/scripts/.zshrc


# Setup GNUPG
mkdir ~/.gnupg && chmod 700 ~/.gnupg && cd ~/.gnupg
ln -s ~/projects/scripts/gpg-agent.conf


# Setup PinEntry GUI
unlink /usr/local/bin/pinentry; ln -s $(which pinentry-mac) /usr/local/bin/pinentry


# Setup config files
mkdir ~/.config
cd ~/.config
ln -s ~/projects/scripts/linux/config/kitty


# Setup Emacs
cd ~/
ln -s ~/projects/scripts/.emacs.d


# Sublime CLI
ln -s "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" /usr/local/bin/subl


# asdf - Check latest version at https://github.com/asdf-vm/asdf
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.0


# Elixir-LS
cd ~/projects/others
git clone https://github.com/elixir-lsp/elixir-ls.git
cd elixir-ls
mix do deps.get, compile, elixir_ls.release
