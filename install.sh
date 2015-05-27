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
        link_file .zshrc
    fi
	link_file .zshenv
	link_file .zalias
	link_file .zprofile

	link_file .emacs.d
	link_file .vimrc
	link_file .gvimrc

	link_file .gitconfig
	link_file .gitignore

	link_file .tmux.conf
	link_file battery
}

install_homebrew()
{
	if ! command -v brew >/dev/null 2>&1; then
		echo "Installing Homebrew..."
		ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
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
	brew install tmux
	brew install zsh
	brew install emacs --cocoa --srgb

	brew install vim
	brew tap neovim/neovim
	brew install --HEAD neovim

	brew install rbenv ruby-build
	rbenv install 2.1.5
	rbenv system 2.1.5
	gem install bundler
	rbenv rehash
}

install_homebrew
install_packages
install_zsh
install_zsh_theme
link_files


