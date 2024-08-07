# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="machuga-avit"

export UPDATE_ZSH_DAYS=14
DISABLE_AUTO_TITLE="true"

COMPLETION_WAITING_DOTS="true"

source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
fpath=($(brew --prefix)/share/zsh-completions $fpath)

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Load Base16
if [[ $(command -v tinty) ]]; then
  tinty init
else
  echo "Tinty not installed. Cannot apply base16 theme"
  brew tap tinted-theming/tinted
  brew install tinty
  tinty install
  tinty apply base16-tomorrow-night
  tinty init
fi

os=`uname -s`

# Vi mode
#bindkey -v

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

[[ -s $HOME/.zshenv ]] && source $HOME/.zshenv

[ -s $HOME/.zalias ] && source $HOME/.zalias

[ -s $HOME/.zsh_local ] && source $HOME/.zsh_local

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -s `brew --prefix`/etc/profile.d/z.sh ] && source `brew --prefix`/etc/profile.d/z.sh

function mkalias() {
    echo "alias $1=\"${@:2}\"" >> ~/.zalias
    source ~/.zalias
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
