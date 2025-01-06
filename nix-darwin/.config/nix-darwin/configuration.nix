{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment = {
    systemPackages = with pkgs; [
      bottom
      dust
      fd
      fzf
      git
      gnupg
      mise
      neovim neovim-remote
      python3
      ripgrep
      starship
      stow
      yazi
      zoxide

      # LSPs and Linters
      lua-language-server ruff ruff-lsp vscode-langservers-extracted vtsls
    ];

    darwinConfig = "$HOME/.config/nix-darwin/configuration.nix";
  };

  fonts.packages = with pkgs; [
    nerd-fonts.hack
  ];

  programs = {
    direnv = {
      enable = true;
      loadInNixShell = true;
      silent = true;
    };

    nix-index.enable = true;

    tmux = {
      enable = true;
      enableFzf = true;
      enableVim = true;
    };

    zsh = {
      enable = true;
      enableCompletion = true;
      # enableFastSyntaxHighlighting = true;
      enableFzfCompletion = true;
      enableFzfHistory = true;
      enableGlobalCompInit = true;
      enableSyntaxHighlighting = true;
    };
  };

  homebrew = {
    enable = true;
    brews = [
      "deno"
      { name = "gmic"; args = ["with-gimp"]; }
      { name = "syncthing"; start_service = true; }
      "ollama"

      # Work
      # { name = "postgresql"; start_service = true; }
      # { name = "redis"; start_service = true; }
      # { name = "elastic/tap/elasticsearch-full"; start_service = true; }
      { name = "colima"; start_service = true; }
      "imagemagick" "libpq" "libyaml" "puma/puma/puma-dev" "vips"
      "docker" "docker-compose"
    ];
    casks  = [
      "duckduckgo"
      "gimp"
      "google-chrome"
      "hammerspoon"
      "localsend"
      "signal"
      "wezterm"

      "Gather"
      "Slack"
      "zed"
    ];
    masApps = {
    };
    taps = [
      "homebrew/services"
    ];
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
  };

  # networking = {
  #   Need to configure: networking.knownNetworkServices
  #   dns = [
  #     "8.8.8.8"
  #     "8.8.4.4"
  #     "2001:4860:4860::8888"
  #     "2001:4860:4860::8844"
  #   ];
  # };

  security = {
    pam.enableSudoTouchIdAuth = true;
  };

  services = {
  };

  system = {
    defaults = {
      controlcenter = {
        BatteryShowPercentage = true;
        Sound = true;
      };

      dock = {
        autohide = true;
        magnification = true;
        largesize = 50;
        persistent-apps = [
          "/Applications/Safari.app"
          "/Applications/Slack.app"
          "/Applications/WezTerm.app"
        ];
        orientation = "bottom";
        tilesize = 24;
      };
      finder = {
        QuitMenuItem = true;
        ShowPathbar = true;
        ShowStatusBar = true;
      };
      trackpad = {
        Clicking = true;
        Dragging = true;
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;
}
