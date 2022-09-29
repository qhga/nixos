{ config, pkgs, user, dotf, ... }:

{
  # TODO: Update to version 0.11 and check if glyphs render correctly
  programs.alacritty = {
    enable = true;
    settings = {
      env.TERM = "xterm-256color";
      window = {
        decorations = "full";
      };
      font = {
        normal = {
          family = "Ttyp0";
          style = "Regular";
        };
        bold = {
          family = "Ttyp0";
          style = "Bold";
        };
        italic = {
          family = "Ttyp0";
          style = "Italic";
        };
        size = 17.0;
      };
      colors = {
        primary = {
          background = "#0d1f2d";
          foreground = "#c3c9e9";
        };
        normal = {
          black   = "#0d1f2d";
          red     = "#ed474a";
          green   = "#a5cc69";
          yellow  = "#fdaa3a";
          blue    = "#64bfd6";
          magenta = "#b95f8a";
          cyan    = "#a2dfed";
          white   = "#c3c9e9";
        };
        bright = {
          black   = "#0d1f2d";
          red     = "#ed474a";
          green   = "#a5cc69";
          yellow  = "#fdaa3a";
          blue    = "#64bfd6";
          magenta = "#b95f8a";
          cyan    = "#a2dfed";
          white   = "#c3c9e9";
        };
      };
    };
  };
}