set PATH ~/.local/bin ~/.cargo/bin $PATH

if status is-interactive
  set -U fish_greeting
  set -g fish_transient_prompt 1

  set -gx GO_PATH "$HOME/.local/share/go"
  set -gx NPM_CONFIG_IGNORE_SCRIPTS "true"

  if type -q fdfind
    alias fd fdfind
  end

  alias re 'exec $SHELL'
  alias rm 'rm -i'

  if type -q timew
    alias tt='timew'
    alias tts='timew summary'
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

  function clip
    base64 | tr -d '\n' | xargs -I{} printf '\033Ptmux;\033\033]52;c;{}\007\033\\'
  end

  fish_config prompt choose astronaut

  if test -f ~/.local.fish
    source ~/.local.fish
  end
end
