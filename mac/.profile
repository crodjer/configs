export PATH=/opt/homebrew/bin:$HOME/.local/bin:$PATH

if [ -e "$HOME/.profile.local" ]; then
    source "$HOME/.profile.local"
fi
