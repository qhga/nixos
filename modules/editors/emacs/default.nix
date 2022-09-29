{ config, pkgs, user, dotf, ... }:

{

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  home.packages = with pkgs; [
    # Dependencies
    ripgrep
    # Latex
    pandoc auctex texlive.combined.scheme-full # includes biber
    python310Packages.pygments
    # Diagrams
    plantuml graphviz
    # Dictionaries
    aspell aspellDicts.en aspellDicts.de

    # :::::::::::::: CODING ::::::::::::::
    # Python
     python311 nodePackages.pyright
    # C/C++
    clang ccls
    llvm libllvm
    # Microcontroller
    platformio avrdude
  ];

  services.emacs = {
    enable = true;
    defaultEditor = true;
  };

  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/emacs";
}