{ config, pkgs, user, dotf, ... }:
let
  t = (import ../../../themes).shanty;
in {
  programs = {
    rofi = {
      enable = true;
      font = t.font + " 14";
      extraConfig = {
        modi = "drun";
        lines = 8;
        show-icons = true;
        icon-theme = "Papirus";
        sort = true;
        sorting-method = "fzf";
        case-sensitive = false;
        hide-scrollbar = false;
        scrollbar-width = 0;
      };
      theme =
        let
          inherit (config.lib.formats.rasi) mkLiteral;
        in {
          "*" = {
            c-accent = mkLiteral t.blue;
            c-border = mkLiteral t.foregroundP1;
            c-bg = mkLiteral t.backgroundP1;
            c-fg = mkLiteral t.foreground;
            background-color = mkLiteral "@c-bg";
            color = mkLiteral "@c-fg";
            width = mkLiteral "500px";
          };

          "inputbar" = {
            children = mkLiteral "[entry]";
            border-color = mkLiteral "@c-border";
          };

          "entry" = {
            padding = mkLiteral "15px";
          };

          "listview" = {
            cycle = mkLiteral "true";
            scrollbar = mkLiteral "false";
            margin = mkLiteral "0 0 -1px 0";
          };

          "element" = {
            padding = mkLiteral "15px";
            border = mkLiteral "0 0 1px 0";
            border-color = mkLiteral "@c-border";
          };

          "element selected" = {
            background-color = mkLiteral "@c-accent";
            color = mkLiteral "@c-bg";
          };

          "element-icon" = {
            size = mkLiteral "1.65ch";
            background-color = mkLiteral "inherit";
          };

          "element-text" = {
            background-color = mkLiteral "inherit";
            text-color = mkLiteral "inherit";
          };
        };
    };
  };
}