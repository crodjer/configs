{ config, lib, pkgs, ... }:

{
  imports = [
  ]
  ++ lib.optional (builtins.pathExists /opt/nix/local.nix) /opt/nix/local.nix;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment = {
    systemPackages = with pkgs; [
      asitop bottom
      dust
      fd
      ffmpeg
      fzf
      git
      gnupg
      mise
      neovim neovim-remote
      ripgrep
      rsync
      starship
      stow
      watch
      yazi
      zoxide

      # Languages
      deno
      python312 pipx ruff ruff-lsp
      lua-language-server
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
      "batt"
      { name = "syncthing"; start_service = true; }

      # Work
      { name = "colima"; start_service = false; }
      "imagemagick" "libpq" "libyaml" "puma/puma/puma-dev" "vips"
      "docker" "docker-compose"
    ];
    casks  = [
      "chromedriver"
      "firefox"
      "gimp"
      "ghostty"
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
