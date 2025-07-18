{ config, lib, pkgs, ... }:

let 
  update-system = pkgs.writeShellScriptBin "update" (builtins.readFile ./scripts/update.sh);
  cleanup-system = pkgs.writeShellScriptBin "clean-os" (builtins.readFile ./scripts/cleanup.sh);
in {
  imports = [
  ]
  ++ lib.optional (builtins.pathExists /opt/nix/local.nix) /opt/nix/local.nix;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment = {
    systemPackages = with pkgs; [
      aria2
      ansible
      asitop
      bat
      cleanup-system
      dust
      inetutils
      fd
      ffmpeg
      fzf
      git
      gnupg
      helix
      jq
      jujutsu
      mactop
      moreutils
      mosh
      (neovim.override {
       configure = {
         customRC = ''
         let $MYVIMRC = expand('~/.config/nvim/init.vim')
         silent! source $MYVIMRC
         '';
         packages.myVimPackages = with pkgs.vimPlugins; {
           start = with vimPlugins; [
             fzf-vim
             nvim-lspconfig
             windsurf-vim           # Needed for work.
           ];
         };
       };
      })
      neovim-remote
      pandoc
      pass
      ripgrep
      rsync
      starship
      stow
      typst
      update-system
      watch
      yazi
      zoxide

      # Languages
      python312 pipx
      lua-language-server
      ansible-language-server ansible-lint
    ];

    darwinConfig = "/Users/rohan/.config/nix-darwin/configuration.nix";
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
      enableFzfCompletion = true;
      enableFzfHistory = true;
      enableGlobalCompInit = true;
      enableSyntaxHighlighting = true;
    };
  };

  homebrew = {
    enable = true;
    brews = [
      "coreutils"
      "batt"
      "deno"
      "gsed"
      "mise"
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
        "/run/current-system/sw/bin/mactop"
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
    primaryUser = "rohan";
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;
}
