##
# Move ssh keys before running
##

# Brew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"


#rbenv
brew update


# Install Kitty
brew cask install kitty


# Install apps
brew install tmux reattach-to-user-namespace emacs


# Projects
mkdir ~/projects
cd ~/projects

git clone git@github.com:scudelletti/scripts.git


# Setup dotfiles
cd ~/
ln -s ~/projects/scripts/.gitconfig
ln -s ~/projects/scripts/.gitignore.global .gitignore
ln -s ~/projects/scripts/.asdfrc
ln -s ~/projects/scripts/macos/.bashrc
ln -s ~/projects/scripts/macos/.tmux.conf
ln -s ~/projects/scripts/macos/.profile
ln -s ~/projects/scripts/macos/bin/

# Setup config files
mkdir -p .config
cd ~/.config
ln -s ~/projects/scripts/linux/kitty


# Oh-My-ZSH
brew install zsh
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
cd ~/
rm .zshrc
ln -s ~/projects/scripts/macos/.zshrc


# Setup Emacs
cd ~/
ln -s ~/projects/scripts/.emacs.d


# Sublime CLI
ln -s "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" /usr/local/bin/subl


# asdf - Check latest version at https://github.com/asdf-vm/asdf
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.7.5


# Elixir-LS
cd ~/projects/others
git clone https://github.com/JakeBecker/elixir-ls.git
cd elixir-ls
mix do deps.get, compile, elixir_ls.release
