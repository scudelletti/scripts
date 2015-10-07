# Brew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

#RVM
brew install gpg
gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
\curl -sSL https://get.rvm.io | bash -s stable
rvm instal 2.2

# Projects
cd ~/
mkdir projects
cd projects

git clone git@github.com:scudelletti/scripts.git

ln -s ~/projects/scripts/.gitconfig
ln -s ~/projects/scripts/.gitignore
ln -s ~/projects/scripts/.pryrc
ln -s ~/projects/scripts/.irbrc
ln -s ~/projects/scripts/macos/.bashrc
ln -s ~/projects/scripts/macos/.zshrc
ln -s ~/projects/scripts/macos/.zshrc
ln -s ~/projects/scripts/macos/.tmux.conf
ln -s ~/projects/scripts/macos/.profile

# Tmux
brew install tmux
brew install reattach-to-user-namespace
# Add powerline-font repo
# Add tmux-powerline repo

# Emacs
brew install --with-cocoa --srgb emacs
cd ~/
ln -s /Users/scudelletti/projects/scripts/.emacs.d

# Sublime Text 3
brew cask install sublime-text3
ln -s "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" ~/bin/subl

# Oh-My-ZSH
brew install zsh
chsh -s /bin/zsh
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
cd /Users/scudelletti/.oh-my-zsh/themes
ln ~/projects/scripts/macos/scudelletti.zsh-theme

cd ~/.oh-my-zsh/
git remote add personal git@github.com:scudelletti/scripts.git
git fetch personal

# Apps
brew install caskroom/cask/brew-cask
brew cask install google-chrome
brew cask install iterm2
brew cask install sourcetree
brew cask install virtualbox
brew cask install vagrant
brew cask install telegram
brew cask install alfred
brew cask install dropbox
brew cask install skype
brew cask install spectacle
brew cask install caffeine
brew cask install google-drive
