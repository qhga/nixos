{ config, pkgs, user, dotf, ... }:

{
  home = {
    username = "phga";
    homeDirectory = "/home/phga";
  };

  # NixPkgs that should be installed to the user profile
  home.packages = with pkgs; [
    # Dependencies for scripts
    xtitle pciutils
    # Essentials
    btop emacs qutebrowser alacritty rofi ripgrep
    # Audio
    mumble lxqt.pavucontrol-qt qpwgraph
  ];


  services.emacs = {
    enable = true;
    defaultEditor = true;
  };

  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/emacs";
  xdg.configFile."qutebrowser".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/qutebrowser";
  xdg.configFile."rofi".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/rofi";
  xdg.configFile."bspwm".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/bspwm";
  xdg.configFile."sxhkd".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/sxhkd";

  # I added this by hand, because I never installed standalone home-manager
  home.stateVersion = "22.11";

  # Let home-manager manage itself
  programs.home-manager.enable = true;
}
