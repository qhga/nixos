{ config, pkgs, user, dotf, ... }:

{

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    # nix-direnv.enableFlakes = true; # TODO: Check this out (external flakes for shell-conf)
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
    gcc
    # llvm libllvm
    # Microcontroller
    # platformio avrdude
    # Rust
    # rustc cargo rustfmt rust-analyzer clippy
    # Otherwise rustic org codeblocks do not work
    rustup # Check l-rust.el for further instructions
    # SQL
    mariadb-client
  ];

  services.emacs = {
    enable = true;
    defaultEditor = true;
  };

  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${dotf}/config/emacs";
}