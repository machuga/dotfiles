# Clear and set the path
export N_PREFIX=$HOME/.n

PATH=""
PATH=$HOME/homebrew/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:.:/bin:$HOME/bin:$HOME/.emacs.d/bin:$HOME/.config/emacs/bin
PATH=$HOME/homebrew/lib/ruby/gems/3.0.0/bin:$PATH
PATH="/Users/machuga/homebrew/opt/grep/libexec/gnubin:$PATH"
PATH=$HOME/.rbenv/shims:$PATH # rbenv
PATH=$N_PREFIX/bin:$PATH
export PATH
