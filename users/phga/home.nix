{ config, pkgs, user, dotf, ... }:

{
  home = {
    username = "phga";
    homeDirectory = "/home/phga";
  };

  # NixPkgs that should be installed to the user profile
  home.packages = with pkgs; [
    # Dependencies for scripts
    xtitle pciutils xdo acpi libnotify dunst tree
    # Essentials
    btop emacs qutebrowser alacritty rofi ripgrep lemonbar-xft trayer pinentry-qt
    # Audio
    mumble lxqt.pavucontrol-qt qpwgraph
    # Social
    signal-desktop
  ];

  qt.style.name = "cleanlooks";

  services = {
    emacs = {
      enable = true;
      defaultEditor = true;
    };
    spotifyd = {
      enable = true;
      settings = {
        global = {
          username = "nördpol";
          password_cmd = "${pkgs.pass}/bin/pass apps/spotifyd | ${pkgs.coreutils}/bin/head -1";
          use_keyring = false;
          initial_volume = "30";
        };
      };
    };
  };

  # This is evil
  home.file.".bashrc".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/bash/bashrc";
  home.file.".bash_aliases".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/bash/bash_aliases";
  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/emacs";
  home.file.".Xresources".source = ../../config/xsession/.Xresources;

  xdg.configFile."qutebrowser".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/qutebrowser";
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
