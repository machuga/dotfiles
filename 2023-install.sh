#!/bin/sh

DOTFILES_DIR=~/dotfiles

install_homebrew()
{
  echo "Checking to see if homebrew needs installed..."
  if ! command -v brew >/dev/null 2>&1; then
    echo "Installing Homebrew..."

    # Install Homebrew to home directory
    mkdir $HOME/homebrew
    curl -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C $HOME/homebrew
    export PATH=$HOME/homebrew/bin:$PATH
    echo "Done."
  else
    export PATH=$HOME/homebrew/bin:$PATH

    echo "Skipping. Homebrew already installed..."
  fi
}

install_homebrew_packages()
{
  echo "Installing homebrew packages..."

  brew tap homebrew/cask-fonts
  brew install alacritty
  brew install bat
  brew install deno
  brew install fzf
  brew install git
  brew install git-delta
  brew install gh
  brew install n
  brew install neovim
  brew install rbenv
  brew install ripgrep
  brew install tmux
  brew install zsh
  brew install zsh-autosuggestions
  brew install zsh-completions
  brew install coreutils fd
  brew install --cask 1password
  brew install --cask alfred
  brew install --cask firefox
  brew install --cask font-inconsolata-g-for-powerline
  brew install --cask spotify
  brew install --cask slack
  brew install --cask rectangle
  brew install --cask zoom
  brew install --cask krisp

  brew tap railwaycat/emacsmacport
  brew install emacs-mac --with-modules

  echo "Done"
}

set_fast_key_repeat()
{
  defaults write NSGlobalDomain KeyRepeat -int 1
}

install_omz()
{
    echo "Installing Oh-My-ZSH..."

    if [ ! -d "$HOME/.oh-my-zsh" ]; then
      sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    else
      echo "Already installed."
    fi

}

install_xcode_tools()
{
  if [ $(xcode-select -p 1>/dev/null;echo $?) -ne "0" ]; then
    xcode-select --install
  fi
}

link_file()
{
    if [[ $2 ]]; then
        local path=$2
    else
        local path=~/$1
    fi

    if [ -L $path ]; then
        echo "Unlinking old $path"
        unlink $path
    fi

    if [ ! -L $path ]; then
        echo "Linking $path to $DOTFILES_DIR/$1..."
        ln -s $DOTFILES_DIR/$1 $path
    fi

    echo ""
}

link_dotfiles()
{
    echo "Linking dotfiles..."

    unlink ~/.zshrc
    link_file .zshrc
    link_file .zshenv
    link_file .zalias
    link_file .zprofile

    #link_file .emacs.d
    echo "Creating ~/.config/nvim directory"
    mkdir -p ~/.config/nvim
    link_file init.vim ~/.config/nvim/init.vim
    link_file .vimrc_background

    mkdir -p ~/.config/alacritty
    link_file alacritty.yml ~/.config/alacritty/alacritty.yml

    link_file .gitconfig
    link_file .gitignore

    link_file .tmux.conf

    echo "Creating ~/bin directory"
    mkdir -p ~/bin
    link_file battery ~/bin/battery
    #link_file .osx
}

fetch_dotfiles()
{
    echo "Checking to see if dotfiles need fetched..."

    if ! [ -r ~/dotfiles ]; then
        echo "Cloning dotfiles..."
        # Fetch dotfiles
        git clone https://github.com/machuga/dotfiles ~/dotfiles
        echo "Done. Installed to ~/dotfiles."
    else
        echo "Dotfiles already downloaded"
    fi
}

install_dotfiles()
{
    echo "*** Installing dotfiles... ***"

    fetch_dotfiles
    link_dotfiles

    # Set some defaults of macOS
    #source ~/.osx

    echo "Done"
    source ~/.zshrc
}

install_shell_theme()
{
    echo "Installing ZSH Theme"
    link_file machuga-avit.zsh-theme ~/.oh-my-zsh/themes/machuga-avit.zsh-theme
    source ~/.zshrc
    echo "Done"
}

install_vim_plug()
{
  sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
}

install_doom_emacs()
{
    if ! [ -r ~/.config/emacs ]; then
      echo "Installing Doom Emacs..."
      git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
      ~/.config/emacs/bin/doom install
      echo "Done!"
    else
        echo "Doom Emacs already installed"
    fi
}


echo "*** Running installer... ***"
install_xcode_tools
install_omz
install_homebrew
install_homebrew_packages
install_shell_theme
install_dotfiles
install_vim_plug
install_doom_emacs
set_fast_key_repeat

