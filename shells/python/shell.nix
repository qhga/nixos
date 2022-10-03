{ pkgs ? import <nixpkgs> {} }:

let
  mach-nix = import (builtins.fetchGit {
    url = "https://github.com/DavHau/mach-nix";
    ref = "refs/tags/3.5.0";
  }) {};

  customPython = mach-nix.mkPython {
    python = "python310";
    requirements = ''
    flask
    '';
  };
in

pkgs.mkShell {
  buildInputs = [ customPython pkgs.nodePackages.pyright ];
}