export PYTHONPATH="$HOME/.local/lib/python/site-packages:$PYTHONPATH"

# Kakoune as terminal editor
export EDITOR=kak
export VISUAL=kak

# PATH exports
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/scripts:$PATH"
export PATH=$HOME/.nix-profile/bin:$PATH

export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/texlive/2021/bin/universal-darwin:$PATH

# Brew shellenv
eval "$(/opt/homebrew/bin/brew shellenv)"

# Use gnu make
export PATH="/opt/homebrew/opt/make/libexec/gnubin:$PATH"

# Postgres12
export PATH="/opt/homebrew/opt/postgresql@12/bin:$PATH"

# Iterm shell integration with tmux
export ITERM_ENABLE_SHELL_INTEGRATION_WITH_TMUX=YES
