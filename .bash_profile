#
# ~/.bash_profile
#

export GOPRIVATE="github.com/curvegrid/*"

export EDITOR=kak
export VISUAL=kak

export PYTHONPATH="$HOME/.local/lib/python/site-packages:$PYTHONPATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/scripts:$PATH"
export PATH="$HOME/.luarocks/bin:$PATH"

export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
