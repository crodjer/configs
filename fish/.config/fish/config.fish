if status is-interactive
  set -U fish_greeting
  set -g fish_transient_prompt 1
  
  abbr --add j " jrnl"
  abbr --add ts "tmux -u new -As $hostname"

  alias b biip
  alias re 'exec $SHELL'
  alias rm 'rm -i'
  alias t timew
  alias tw task
  alias fd fdfind

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
