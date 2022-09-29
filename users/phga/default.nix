{ config, pkgs, user, dotf, ... }:

{

  imports = [
    ../../modules/editors/emacs
    ../../modules/desktop/dunst
    ../../modules/desktop/alacritty
    ../../modules/browser/qutebrowser
  ];

  home = {
    username = "phga";
    homeDirectory = "/home/phga";
    pointerCursor = {
      x11.enable = true;
      name = "Bibata-Original-Amber";
      package = pkgs.bibata-cursors;
      size = 16;
    };
  };

  qt.style.name = "cleanlooks";

  # NixPkgs that should be installed to the user profile
  home.packages = with pkgs; [
    # Dependencies for scripts
    xtitle pciutils xdo acpi tree xclip
    # Essentials
    pass # Password manager
    ripgrep # Grep alternative
    pinentry-qt
    btop # Process explorer
    maim # Screenhots
    p7zip # All in one compression program
    fzf
    # Desktop environment
    papirus-icon-theme
    lemonbar-xft trayer
    rofi
    xfce.thunar
    gnome.file-roller
    # Browser
    firefox
    chromium
    # PDF & Writing
    xournalpp
    # Audio
    mumble # Voice chat
    lxqt.pavucontrol-qt qpwgraph
    # Image/Video
    nomacs # Image viewer
    xcolor # Color picker
    obs-studio # Screenrecording
    yt-dlp # Youtube DL (To stream via mpv)
    mpv # Video player
    inkscape # Vector graphics
    krita # Digital art
    darktable # Photo editor
    # Social
    signal-desktop
    # Niche programs
    gbdfed # Bitmap font editor
    # Network tools
    nmap whois
  ];

  services = {
    # TODO: wait for gnupg agent or use keyring
    spotifyd = {
      enable = true;
      settings = {
        global = {
          username = "nördpol";
          password_cmd = "${pkgs.pass}/bin/pass apps/spotifyd | ${pkgs.coreutils}/bin/head -1";
          use_keyring = false;
          initial_volume = "40";
        };
      };
    };
  };

  # This is evil
  home.file.".bashrc".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/bash/bashrc";
  home.file.".bash_aliases".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/bash/bash_aliases";

  xdg.configFile."rofi".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/rofi";
  xdg.configFile."bspwm".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/bspwm";
  xdg.configFile."sxhkd".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/sxhkd";
  xdg.dataFile."applications/pw.desktop".source =
    config.lib.file.mkOutOfStoreSymlink "${dotf}/scripts/rofi_pass/pw.desktop";
  xdg.dataFile."fonts/otb".source = ../../config/fonts/otb;

  # I added this by hand, because I never installed standalone home-manager
  home.stateVersion = "22.11";

  # Let home-manager manage itself
  programs.home-manager.enable = true;
}
