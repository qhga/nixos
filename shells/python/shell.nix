{ pkgs ? import <nixpkgs> {} }:

let
  mach-nix = import (builtins.fetchGit {
    url = "https://github.com/apeschar/mach-nix";
    ref = "distlib-upgrade";
    # url = "https://github.com/DavHau/mach-nix";
    # ref = "refs/tags/3.5.0";
  }) {
    python = "python310";
    # We always have to provide both the commit-hash from https://github.com/DavHau/pypi-deps-db
    # and the Hash that is printed when we just provide a random hash in pypyDataSha256
    pypiDataRev = "9816a174b2cb509500d38ead436a2938e6f2dad1";
    pypiDataSha256 = "sha256:1a3glnkrfp2k543ralahyn3cmz6axk3ihf18rbygg6dg8hg0pg0g";
  };
  customPython = mach-nix.mkPython {
    requirements = ''
    pandas == 1.5.1
    # seaborn
    '';
  };
in

pkgs.mkShell {
  buildInputs = [ customPython pkgs.nodePackages.pyright ];
}