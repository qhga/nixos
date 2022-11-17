{ config, pkgs, user, dotf, ... }:

let
  t = (import ../../../themes).shanty;
in {
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
          family = t.font;
          style = "Regular";
        };
        bold = {
          family = t.font;
          style = "Bold";
        };
        italic = {
          family = t.font;
          style = "Italic";
        };
        size = 11.0;
      };
      colors = {
        primary = {
          background = t.background;
          foreground = t.foreground;
        };
        normal = {
          black   = t.black;
          red     = t.red;
          green   = t.green;
          yellow  = t.yellow;
          blue    = t.blue;
          magenta = t.magenta;
          cyan    = t.cyan;
          white   = t.white;
        };
        bright = {
          black   = t.black;
          red     = t.red;
          green   = t.green;
          yellow  = t.yellow;
          blue    = t.blue;
          magenta = t.magenta;
          cyan    = t.cyan;
          white   = t.white;
        };
      };
    };
  };
}