{ config, pkgs, user, dotf, ... }:

{
  home = {
    username = "phga";
    homeDirectory = "/home/phga";
  };

  # NixPkgs that should be installed to the user profile
  home.packages = with pkgs; [
    btop emacs qutebrowser alacritty mumble rofi
  ];
  
  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/emacs";
  xdg.configFile."qutebrowser".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/qutebrowser";

  # I added this by hand, because I never installed standalone home-manager
  home.stateVersion = "22.11";

  # Let home-manager manage itself
  programs.home-manager.enable = true;
}
