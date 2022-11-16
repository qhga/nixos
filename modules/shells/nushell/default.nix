{ config, pkgs, user, dotf, ... }:
let
  t = (import ../../themes).shanty;
in {
  programs = {
    nushell = {
      enable = true;
    };
  };
}