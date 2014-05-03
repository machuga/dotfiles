PATH=$HOME/bin:/usr/local/bin:/usr/bin:.:/usr/local/share/npm/bin:/bin:$PATH

[[ $os = "Darwin" ]] && PATH="/Applications/Postgres.app/Contents/MacOS/bin":$PATH

if [ -s $HOME/.rvm/scripts/rvm ]; then
    PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
else
    PATH=$HOME/.rbenv/shims:$PATH
fi
