{ config, pkgs, user, dotf, ... }:
let
  t = (import ../../../themes).shanty;
in {
  programs.thunderbird = {
    enable = true;
    profiles = {
      first = {
        isDefault = true;
        withExternalGnupg = true;
      };
    };
  };
}