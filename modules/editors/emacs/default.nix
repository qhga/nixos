{ config, pkgs, user, dotf, ... }:

{

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  home.packages = with pkgs; [
    # Dependencies
    ripgrep nodePackages.pyright python311
  ];

  services.emacs = {
    enable = true;
    defaultEditor = true;
  };

  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/emacs";
}