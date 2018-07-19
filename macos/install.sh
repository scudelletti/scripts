##
# Apps
##
# brew cask install google-chrome
# brew cask install iterm2
# brew cask install sourcetree
# brew cask install virtualbox
# brew cask install vagrant
# brew cask install telegram
# brew cask install alfred
# brew cask install dropbox
# brew cask install skype
# brew cask install spectacle
# brew cask install caffeine
# brew cask install google-drive
# brew cask install sublime

##
# Move ssh keys before running
##

# Brew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"


#rbenv
brew update
brew install rbenv


# Projects
mkdir ~/projects
cd ~/projects

git clone git@github.com:scudelletti/scripts.git


# Setup dotfiles
cd ~/
ln -s ~/projects/scripts/.gitconfig
ln -s ~/projects/scripts/.gitignore
ln -s ~/projects/scripts/.pryrc
ln -s ~/projects/scripts/.irbrc
ln -s ~/projects/scripts/.asdfrc
ln -s ~/projects/scripts/macos/.bashrc
ln -s ~/projects/scripts/macos/.tmux.conf
ln -s ~/projects/scripts/macos/.tmux-powerlinerc
ln -s ~/projects/scripts/macos/.profile

# Tmux
brew install tmux
brew install reattach-to-user-namespace

# Tmux Powerline
mkdir ~/projects/others
cd ~/projects/others
git clone git@github.com:scudelletti/tmux-powerline.git

git clone git@github.com:scudelletti/powerline-fonts.git
cd ~/projects/others/powerline-fonts
git fetch origin
git checkout production
# Now you need to install the fonts


# Emacs
brew install --with-cocoa --srgb emacs
cd ~/
ln -s ~/projects/scripts/.emacs.d


# Sublime CLI
ln -s "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" /usr/local/bin/subl


# Oh-My-ZSH
brew install zsh
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
cd ~/
rm .zshrc
ln -s ~/projects/scripts/macos/.zshrc
