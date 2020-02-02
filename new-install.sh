#!/bin/bash

DOTFILES_DIR=~/dotfiles
install_homebrew()
{
    echo "Checking to see if homebrew needs installed..."
    if ! command -v brew >/dev/null 2>&1; then
        echo "Installing Homebrew..."

        # Install Homebrew to home directory
        mkdir homebrew && curl -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C homebrew
        export PATH=$HOME/homebrew/bin:$PATH
        echo "Done."
    else
        echo "Skipping. Homebrew already installed..."
    fi
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

    link_file .zshrc
    link_file .zshenv
    link_file .zalias
    link_file .zprofile

    #link_file .emacs.d
    echo "Creating ~/.config/nvim directory"
    mkdir -p ~/.config/nvim
    link_file .vimrc ~/.config/nvim/init.vim

    link_file .gitconfig
    link_file .gitignore

    link_file .tmux.conf

    echo "Creating ~/bin directory"
    mkdir -p ~/bin
    link_file battery ~/bin/battery
    link_file .osx
}

install_dotfiles()
{
    echo "*** Installing dotfiles... ***"

    fetch_dotfiles
    link_dotfiles

    # Set some defaults of macOS
    #source ~/.osx

    echo "Done"
}


containsElement () {
    if [[ $2 =~ \w?"$1"\w? ]]; then
        return 0;
    else
        return 1;
    fi
}

install_package()
{
    #	echo "Checking to see if '$1' needs installed..."
    if containsElement "$1" "$2"; then
        echo "Package was installed"
        echo "Package '${1}' already installed. Skipping."
    else
        echo "Package needs installed"
        echo "Installing package '${1}' with $(echo "brew install ${1}")"
        brew install $1
        echo "Done."
    fi
}

install_cask_package()
{

#	echo "Checking to see if '$1' needs installed..."
if containsElement "$1" "$2"; then
    echo "Package was installed"
    echo "Package '${1}' already downloaded. Skipping."
else
    echo "Package needs installed"
    echo "Installing cask package '${1}' with '$(echo "brew cask install ${1}")'"
    brew cask install $1
    echo "Done."
fi

}

install_packages()
{

    echo ""
    echo "*** Installing packages... ***"

    # Allow font installations
    echo "*** Tapping cask-fonts... ***"
    brew tap homebrew/cask-fonts

    local existingPackages="$(brew list)"
    local existingCaskPackages="$(brew cask list)"

    declare -a packages=(
        git
        zsh
        zsh-autosuggestions
        zsh-completions
        fzf
        tmux
        neovim
    )

    declare -a caskPackages=(
        firefox
        iterm2
        1password
        alfred
        slack
        spotify
        font-inconsolata-g-for-powerline
        docker
        visual-studio-code
        spectacle
    )


    echo "*** Installing brew packages... ***"

    for i in "${packages[@]}"
    do
        install_package "$i" "${existingPackages[@]}"
    done

    echo ""
    echo "*** Installing brew cask packages... ***"

    for i in "${caskPackages[@]}"
    do
        install_cask_package "$i" "${existingCaskPackages[@]}"
    done

    echo "All packages installed"

}

install_omz()
{
    echo "Installing Oh-My-ZSH..."
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
}

install_shell_theme()
{
    echo "Installing ZSH Theme"
    link_file machuga-avit.zsh-theme ~/.oh-my-zsh/themes/machuga-avit.zsh-theme
    source ~/.zshrc
    echo "Done"
}

install_base16_shell()
{
    echo "Installing Base 16 Shell"
    if ! [ -r ~/.config/base16-shell ]; then
        git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
    else
        echo "Already installed. Skipping"
    fi

    source ~/.zshrc

    $(base16_tomorrow-night)

    echo "Done"
}

# EXECUTE
echo "Kicking off installation script!"

install_homebrew

echo ""

install_omz

echo ""

install_shell_theme

echo ""

install_packages

echo ""

install_dotfiles

echo ""

install_base16_shell

