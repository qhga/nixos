{ config, pkgs, user, dotf, ... }:
let
  t = (import ../../../themes).shanty;
in {
  services = {
    dunst = {
      enable = true;
      settings = {
        global = {
          monitor = 0;
          follow = "none";
          shrink = "yes";
          transparency = 0;
          separator_height = 2;
          padding = 12;
          horizontal_padding = 12;
          frame_width = 2;
          # separator_color = "#ffffff";
          font = "Ttyp0 15";
          markup = "full";
          format = "<b>%s</b>\n%b";
          history_length = 10;
          icon_position = "left";
          stack_duplicates = "true";
          hide_duplicate_count = "true";
          mouse_left_click = "close_current";
          mouse_middle_click = "do_action, close_current";
          mouse_right_click = "close_all";
        };
        urgency_low = {
          background = t.background_alt;
          foreground = t.foreground;
          frame_color = t.foreground;
          timeout = 5;
        };
        urgency_normal = {
          background = t.background_alt;
          foreground = t.foreground;
          frame_color = t.foreground;
          timeout = 10;
        };
        urgency_critical = {
          background = t.background_alt;
          foreground = t.error;
          frame_color = t.error;
          timeout = 15;
        };
      };
    };
  };

  home.packages = with pkgs; [
    libnotify
  ];
}