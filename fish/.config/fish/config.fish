if status is-interactive
  set -U fish_greeting
  set -g fish_transient_prompt 1

  set -gx GO_PATH "$HOME/.local/share/go"
  
  abbr --add j " jrnl"
  abbr --add ts "tmux -u new -As $hostname"

  if type -q batcat
    # Debian use `batcat` doesn't 
    set bat_cmd batcat
  else
    set bat_cmd bat
  end

  if type -q fdfind
    alias fd fdfind
  end

  alias bat '$bat_cmd --theme ansi'
  alias batp '$bat_cmd --theme ansi -p'
  alias b biip
  alias re 'exec $SHELL'
  alias rm 'rm -i'
  alias t timew
  alias tw task
  alias bun 'firejail --whitelist=$(which bun) --whitelist=$PWD $(which bun)'

  if type -q starship
    eval (starship init fish)
  end
  if type -q direnv
    direnv hook fish | source
  end
  if type -q zoxide
    zoxide init fish | source
  end

  if type -q mise
    mise activate fish | source
  end

  if test -f ~/.local.fish
    source ~/.local.fish
  end
end
