export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"

PATH=""
PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:.:/bin
PATH=/usr/local/share/npm/bin:$HOME/.composer/vendor/bin:$PATH:$GOBIN

[[ $os = "Darwin" ]] && PATH=$PATH:/Applications/Postgres.app/Contents/Versions/9.4/bin
[[ $os = "Darwin" ]] && PATH=$PATH:/Applications/Racket\ v6.6/bin

if [ -s $HOME/.rvm/scripts/rvm ]; then
    PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
else
    PATH=$HOME/.rbenv/shims:$PATH
fi

function load_nvm() {
  if [ -s $HOME/.nvm/nvm.sh ] || [ -s /usr/local/opt/nvm/nvm.sh ]; then
    export NVM_DIR=~/.nvm
    source /usr/local/opt/nvm/nvm.sh
  fi
}


export PATH=$HOME/bin:$HOME/.cargo/bin:$PATH
