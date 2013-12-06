# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="machuga"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

os=`uname -s`

# Customize to your needs...
PATH=/usr/local/bin:/usr/bin:.:/usr/local/share/npm/bin:/bin:$PATH

[[ $os = "Darwin" ]] && PATH="/Applications/Postgres.app/Contents/MacOS/bin":$PATH

if [ -s $HOME/.rvm/scripts/rvm ]; then
    PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
else
    PATH=$HOME/.rbenv/shims:$PATH
fi

PATH="$HOME/.cask/bin:$PATH"

# Environment variables
export EDITOR="vim"
#export LSCOLORS="ExGxBxDxCxEgEdxbxgxcxd"
export GREP_OPTIONS="--color"
export ACK_COLOR_MATCH="red"
export WORDCHARS='*?[]~&;!$%^<>'

# Less Colors for Man Pages
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

if [[ $os = "Darwin" ]]; then
    alias start_pg="pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start"
fi

source $HOME/.zalias

if [ -s $HOME/.private_env ]; then
    source $HOME/.private_env
fi
# Vi mode
#bindkey -v
