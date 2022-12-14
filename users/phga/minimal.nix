{ config, pkgs, user, dotf, ... }:

{

  imports = [
    ../../modules/shells/bash
  ];

  home = {
    username = "phga";
    homeDirectory = "/home/phga";
  };

  # NixPkgs that should be installed to the user profile
  home.packages = with pkgs; [
    whois
  ];

  # This is evil (TEST if it breaks)
  xdg.configFile."rofi".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/rofi";

  # I added this by hand, because I never installed standalone home-manager
  home.stateVersion = "22.11";

  # Let home-manager manage itself
  programs.home-manager.enable = true;
}