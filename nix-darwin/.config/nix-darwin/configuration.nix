{ config, lib, pkgs, ... }:

let 
  update-system = pkgs.writeShellScriptBin "update" (builtins.readFile ./scripts/update.sh);
  cleanup-system = pkgs.writeShellScriptBin "clean-os" (builtins.readFile ./scripts/cleanup.sh);
  initialize = pkgs.writeShellScriptBin "initialize" (builtins.readFile ./scripts/initialize.sh);
in {
  imports = [
  ]
  ++ lib.optional (builtins.pathExists /opt/nix/local.nix) /opt/nix/local.nix;

  environment = {
    darwinConfig = "/Users/rohan/.config/nix-darwin/configuration.nix";

    interactiveShellInit = ''
    export GPG_TTY=`tty`
    '';

    shellAliases = {
      b = "biip";  # My PII Stripping tool!
      mactop = "TERM=xterm sudo mactop";
      re = "exec $SHELL";
      rm = "rm -i";
      t = "timew";
      tw = "task";
    };

    shells = [
      pkgs.fish
      pkgs.zsh
    ];

    systemPackages = with pkgs; [
      aria2
      bat
      cleanup-system
      delta
      dust
      inetutils
      entr
      fd
      ffmpeg
      fzf
      git
      helix
      immich-cli
      jq
      jujutsu
      jrnl
      mactop
      mise
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
             ansible-vim
             fzf-vim
             nvim-lspconfig
             windsurf-vim           # Needed for work.
           ];
         };
       };
      })
      neovim-remote
      nh
      pandoc
      pass
      ripgrep
      rsync
      starship
      stow
      taskwarrior3 timewarrior vit
      typst
      update-system
      watch
      yazi
      zoxide

      # Languages
      ansible ansible-lint
      python312 pipx pyright ruff uv
      lua-language-server
    ];

    systemPath =  [
      "/opt/homebrew/bin"
      "~/.local/bin"
      "~/.cargo/bin"
    ];

    variables = {
      EDITOR = "nvim";
      LESS = "-R";
    };
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

    fish = {
      enable = true;
      shellInit = ''
      set -U fish_greeting
      set -g fish_transient_prompt 1

      abbr --add j " jrnl"
      abbr --add jw " jrnl work"

      ${pkgs.mise}/bin/mise activate fish | source
      ${pkgs.direnv}/bin/direnv hook fish | source
      ${pkgs.zoxide}/bin/zoxide init fish | source
      eval (${pkgs.starship}/bin/starship init fish)

      if test -f ~/.local.fish
        source ~/.local.fish
      end
      '';
    };

    gnupg = {
      agent = {
        enable = true;
        enableSSHSupport = true;
      };
    };

    info.enable = true;
    man.enable = true;

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
      "ansible-language-server"
      "coreutils"
      "batt"
      "gsed"
      { name = "syncthing"; start_service = true; }
      "terminal-notifier"
    ];
    casks  = [
      "firefox"
      "firefox@developer-edition"
      "gimp"
      "ghostty"
      "hammerspoon"
      "localsend"
      "obsidian"
      "signal"
      "wezterm"
    ];
    # greedyCasks = true;
    masApps = {
    };
    taps = [
    ];
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
  };

  launchd = {
    user = {
      envVariables = {
      };
      agents."initialize" = {
        # `initialize` is a custom script which run at login on mac.
        command = "${initialize}/bin/initialize";
        path = [];
        serviceConfig = {
          Label = "environment";
          RunAtLoad = true;
          KeepAlive = false;
        };
      };
    };
  };

  security = {
    pam.services.sudo_local.touchIdAuth = true;
    sudo.extraConfig = let
      commands = [
        "/run/current-system/sw/bin/darwin-rebuild"
        "/run/current-system/sw/bin/mactop"
        "/run/current-system/sw/bin/nix-channel"
        "/run/current-system/sw/bin/nix-collect-garbage"
        "/sbin/mount"
        "/sbin/umount"
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
