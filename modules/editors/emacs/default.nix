{ config, pkgs, user, dotf, ... }:

{
  home.packages = with pkgs; [
    emacs
    # Dependencies
    ripgrep nodePackages.pyright python311
  ];

  services.emacs = {
    enable = true;
    defaultEditor = true;
  };

  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/emacs";
}