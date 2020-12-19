#!/bin/bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

link_file()
{
	local path=~/$1
	echo "Linking $path to $DIR..."
	if [ ! -L $path ]; then
        echo "Linking $path to $DIR/$1..."
		ln -s $DIR/$1 $path
	fi
}

link_files()
{
	echo "Linking any unlinked files"
	if [ -f ~/.zshrc ]; then
		unlink ~/.zshrc
	fi
	link_file .zshrc
	link_file .zshenv
	link_file .zalias
	link_file .zprofile
	link_file .zshrc

	link_file .emacs.d
	link_file .vimrc
	link_file .gvimrc
	link_file .spacemacs

	link_file .gitconfig
	link_file .gitignore

	link_file .tmux.conf
	link_file battery
	link_file .osx
}

install_homebrew()
{
	if ! command -v brew >/dev/null 2>&1; then
		echo "Installing Homebrew..."
		/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
		brew tap caskroom/cask
	else
		echo "Skipping Homebrew..."
	fi

}

install_zsh()
{
	if [ ! -d ~/.oh-my-zsh ]; then
		echo "Installing Oh My ZSH..."
		command -v zsh | sudo tee -a /etc/shells # Add to avail shells
		curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
	else
		echo "Skipping Oh My ZSH..."
	fi
}

install_zsh_theme()
{
	echo "Copying ZSH theme..."
	cp $DIR/machuga-short.zsh-theme ~/.oh-my-zsh/themes/machuga-short.zsh-theme
}

install_packages()
{
	brew cask install google-chrome
	brew cask install firefox
	brew cask install iterm2
  brew cask install alfred
	brew cask install atom
  brew cask install screenhero
	brew cask install screenflow
	brew cask install dropbox
	brew cask install slack
	brew cask install racket
	brew cask install spectacle
	brew cask install spotify
	brew cask install virtualbox

	brew install git
	brew install tmux
	brew install zsh
	brew install fzf
	brew install nvm

	brew install emacs --with-cocoa
	brew install vim
	brew install neovim/neovim/neovim

	brew install rbenv ruby-build
	rbenv install 2.3.3
	rbenv default 2.3.3
	gem install bundler
	rbenv rehash
}

install_256()
{
  git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
  source ~/.zshrc
  base16_tomorrow-night
}

#install_homebrew
#install_packages
#install_zsh
#install_zsh_theme
link_files

# One time source macOS config settings
source ~/.osx
