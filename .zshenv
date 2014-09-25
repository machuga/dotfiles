PATH=""
PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:.:$HOME/bin:/bin

export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"
export PATH=/usr/local/share/npm/bin:$HOME/.composer/vendor/bin:$PATH:$GOBIN:$PATH

[[ $os = "Darwin" ]] && PATH="/Applications/Postgres.app/Contents/MacOS/bin":$PATH

if [ -s $HOME/.rvm/scripts/rvm ]; then
    PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
else
    PATH=$HOME/.rbenv/shims:$PATH
fi
