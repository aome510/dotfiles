#######################################################
# Variables
#######################################################
export EDITOR=hx
export VISUAL=hx

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
# export PATH="$HOME/Library/Python/3.8/bin:$PATH"
# export PATH="$HOME/Library/Python/3.9/bin:$PATH"
## python path
# export PYTHONPATH="$HOME/Library/Python/3.8/lib/site-packages:$PYTHONPATH"
# export PYTHONPATH="$HOME/Library/Python/3.9/lib/site-packages:$PYTHONPATH"

# local exports
export PATH=/usr/local/bin:$PATH
export PATH="$HOME/.local/bin:$PATH"

# pyenv setup
eval "$(pyenv init -)"

# openblas and lapback
# export LDFLAGS="-L/opt/homebrew/opt/openblas/lib:$LDFLAGS"
# export CPPFLAGS="-I/opt/homebrew/opt/openblas/include:$CPPFLAGS"
# export PKG_CONFIG_PATH="/opt/homebrew/opt/openblas/lib/pkgconfig:$PKG_CONFIG_PATH"
# export LDFLAGS="-L/opt/homebrew/opt/lapack/lib:$LDFLAGS"
# export CPPFLAGS="-I/opt/homebrew/opt/lapack/include:$CPPFLAGS"
# export PKG_CONFIG_PATH="/opt/homebrew/opt/lapack/lib/pkgconfig:$PKG_CONFIG_PATH"

# export LDFLAGS="-L/opt/homebrew/lib:$LDFLAGS"
