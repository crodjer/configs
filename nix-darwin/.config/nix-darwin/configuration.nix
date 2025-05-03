{ config, lib, pkgs, ... }:

{
  imports = [
  ]
  ++ lib.optional (builtins.pathExists /opt/nix/local.nix) /opt/nix/local.nix;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment = {
    systemPackages = with pkgs; [
      aria2
      asitop
      bat
      bottom
      dust
      inetutils
      fd
      ffmpeg
      fzf
      git
      gnupg
      helix
      mosh
      ripgrep
      rsync
      starship
      stow
      watch
      yazi
      zoxide

      # Languages
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
      "deno"
      "gsed"
      "mise"
      "neovim" "neovim-remote"
      "nushell"
      { name = "syncthing"; start_service = true; }

      # Work
      { name = "colima"; start_service = false; }
      "imagemagick" "libpq" "libyaml" "puma/puma/puma-dev" "vips"
      "docker" "docker-compose"
    ];
    casks  = [
      "brave-browser"
      "firefox"
      "gimp"
      "ghostty"
      "hammerspoon"
      "localsend"
      "obsidian"
      "signal"
      "thunderbird"

      # Work
      "chromedriver"
      "cursor"
      "zed"
      "windsurf"
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

  security = {
    pam.services.sudo_local.touchIdAuth = true;
    sudo.extraConfig = let
      commands = [
        "/run/current-system/sw/bin/darwin-rebuild"
        "/run/current-system/sw/bin/nix*"
      ];
      commandsString = builtins.concatStringsSep ", " commands;
    in ''
      %admin ALL=(ALL:ALL) NOPASSWD: ${commandsString}
    '';
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
        largesize = 96;
        persistent-apps = [
          "/Applications/Firefox.app"
          "/Applications/Ghostty.app"
        ];
        orientation = "bottom";
        tilesize = 64;
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
