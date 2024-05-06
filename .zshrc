# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="machuga-avit"

export UPDATE_ZSH_DAYS=13
DISABLE_AUTO_TITLE="true"

COMPLETION_WAITING_DOTS="true"

source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
fpath=($(brew --prefix)/share/zsh-completions $fpath)

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
plugins=(git)

source $ZSH/oh-my-zsh.sh

os=`uname -s`

# Customize to your needs...
#
# Vi mode
#bindkey -v
#
[[ -s $HOME/.zshenv ]] && source $HOME/.zshenv

# Load zprofile
#[[ -s $HOME/.zprofile ]] && source $HOME/.zprofile

export EDITOR="vim"
if command -v nvim >/dev/null 2>&1 ; then
    #export VIM="/usr/local/share/vim"
    alias vim="nvim"
    alias vi="nvim"
    export EDITOR="nvim"
fi

export GREP_OPTIONS="--color"
export ACK_COLOR_MATCH="red"
export WORDCHARS='*?[]~&;!$%^<>'
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*" --glob "!node_modules/*"'

# Less Colors for Man Pages
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

[ -s $HOME/.zalias ] && source $HOME/.zalias

[ -s $HOME/.private_env ] && source $HOME/.private_env

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -s $HOME/.auth0-alias ] && source $HOME/.auth0-alias

[ -s `brew --prefix`/etc/profile.d/z.sh ] && source `brew --prefix`/etc/profile.d/z.sh

function mkalias() {
    echo "alias $1=\"${@:2}\"" >> ~/.zalias
    source ~/.zalias
}

function load_nvm() {
  if [ -s $HOME/.nvm/nvm.sh ] || [ -s /usr/local/opt/nvm/nvm.sh ]; then
    export NVM_DIR=~/.nvm
    source /usr/local/opt/nvm/nvm.sh
  fi
}

if command -v fasd >/dev/null 2>&1 ; then
  eval "$(fasd --init auto)"
fi

function pr-checkout() {
  local jq_template pr_number

  jq_template='"'\
'#\(.number) - \(.title)'\
'\t'\
'Author: \(.user.login)\n'\
'Created: \(.created_at)\n'\
'Updated: \(.updated_at)\n\n'\
'\(.body)'\
'"'

  pr_number=$(
    gh api 'repos/:owner/:repo/pulls' |
    jq ".[] | $jq_template" |
    sed -e 's/"\(.*\)"/\1/' -e 's/\\t/\t/' |
    fzf \
      --with-nth=1 \
      --delimiter='\t' \
      --preview='echo -e {2}' \
      --preview-window=top:wrap |
    sed 's/^#\([0-9]\+\).*/\1/'
  )

  if [ -n "$pr_number" ]; then
    gh pr checkout "$pr_number"
  fi
}

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"
# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

[[ ! $(command -v nix) && -e "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" ]] && source "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
path+=("/nix/var/nix/profiles/default/bin")

eval "$(direnv hook zsh)"
# Nix
if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
  source "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi
# End Nix
