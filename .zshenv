#######################################################
# Variables
#######################################################
# Kakoune as terminal editor
export EDITOR=kak
export VISUAL=kak

# Iterm shell integration with tmux
# export ITERM_ENABLE_SHELL_INTEGRATION_WITH_TMUX=YES

# Brew shellenv
eval "$(/opt/homebrew/bin/brew shellenv)"

#######################################################
# PATH
#######################################################
# go
export PATH="$HOME/go/bin:$PATH"

# rust
export PATH="$HOME/.cargo/bin:$PATH"

# yarn
export PATH="$HOME/.yarn/bin:$PATH"

# user-defined scripts
export PATH="$HOME/scripts:$PATH"

# tex/latex
export PATH=/usr/local/texlive/2021/bin/universal-darwin:$PATH

# Use gnu make
export PATH="/opt/homebrew/opt/make/libexec/gnubin:$PATH"

# python
export PATH="$HOME/Library/Python/3.8/bin:$PATH"
export PATH="$HOME/Library/Python/3.9/bin:$PATH"
## python path
# export PYTHONPATH="$HOME/Library/Python/3.8/lib/site-packages:$PYTHONPATH"
# export PYTHONPATH="$HOME/Library/Python/3.9/lib/site-packages:$PYTHONPATH"

# local exports
export PATH=/usr/local/bin:$PATH
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.bin:$PATH"

# pyenv setup
eval "$(pyenv init -)"
