{ config, pkgs, user, dotf, ... }:
let
  t = (import ../../themes).shanty;
in
{

  programs.qutebrowser = {
    enable = true;
    package = pkgs.qutebrowser-qt6;
    loadAutoconfig = true;
    settings = {
      qt.args = [ "widevine-path=${pkgs.vivaldi-widevine}/share/google/chrome/WidevineCdm/_platform_specific/linux_x64/libwidevinecdm.so" ];
      url = {
        start_pages = [ "https://duckduckgo.org/" ];
      };
      fonts = let font = "15px " + t.font; in {
        tabs.selected = font;
        tabs.unselected = font;
        statusbar = font;
        downloads = font;
        prompts = font;
        hints = font;
        messages.info = font;
        keyhint = font;
        messages.warning = font;
        messages.error = font;
        completion.entry = font;
        completion.category = font;
      };
      zoom.default = "100%";
      tabs = {
        favicons.scale = 0.8;
        # padding = {
        #   top = 0; bottom = 0; left = 5; right = 5;
        # };
      };
      # statusbar = {
      # padding = {
      #   top = 0; bottom = 0; left = 5; right = 5;
      # };
      # };
      downloads.remove_finished = 5000;
      content = {
        # javascript.can_access_clipboard = true;
        javascript.clipboard = "access";
        blocking.method = "both";
        blocking.adblock.lists = [
          "https://easylist.to/easylist/easylist.txt"
          "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt"
          "https://secure.fanboy.co.nz/fanboy-annoyance.txt"
          "https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances.txt"
          "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters.txt"
          "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2021.txt"
          "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2022.txt"
          "https://github.com/uBlockOrigin/uAssets/raw/master/filters/unbreak.txt"
          "https://github.com/uBlockOrigin/uAssets/raw/master/filters/resource-abuse.txt"
          "https://github.com/uBlockOrigin/uAssets/raw/master/filters/privacy.txt"
          "https://pgl.yoyo.org/adservers"
          "https://www.i-dont-care-about-cookies.eu/abp/"
        ];
        blocking.hosts.lists = ["https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"];
        blocking.whitelist = ["piwik.org" "*.reddit.com" "*.redditinc.com" "*.redditstatic.com"];
        user_stylesheets = [ "~/.config/qutebrowser/styles/floppy.css" ];
        desktop_capture = "ask"; #"ask";
        javascript.alert = false;
        local_storage = true; # WOW REDDIT AND OTHER WEBSITES
        cookies.store = true;
        default_encoding = "utf-8";
        geolocation = "ask"; # Valid values: true, false, ask
        headers.accept_language = "en-US,en,de";
        headers.custom = {};
        headers.do_not_track = true;
        headers.referer = "same-domain"; # Valid values: always, never, same-domain;
        # headers.user_agent = "Windows-Media-Player/11.0.5721.5145";
        # headers.user_agent = "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:77.0) Gecko/20100101 Firefox/77.0";
        # headers.user_agent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36";
      };
      colors = let
        bgc = t.background;
        bgc_alt = t.backgroundM1;
        fgc = t.foreground;
        errorc = t.error;
        warningc = t.warning;
        selectedc = t.cyan;
        statusc = t.green;
        tabsindicatorc = t.white;
        indicatorc = t.white;
      in {
        completion = {
          fg = fgc;
          odd.bg = bgc;
          even.bg = bgc;
          category.fg = selectedc;
          category.bg = bgc;
          category.border.top = bgc;
          category.border.bottom = bgc;
          item.selected.fg = bgc;
          item.selected.bg = selectedc;
          item.selected.border.top = selectedc;
          item.selected.border.bottom = selectedc;
          match.fg = selectedc;
          scrollbar.fg = fgc;
          scrollbar.bg = bgc;
        };

        downloads = {
          bar.bg = bgc;
          start.fg = bgc;
          start.bg = selectedc;
          stop.fg = bgc;
          stop.bg = statusc;
          error.fg = errorc;
        };

        hints = {
          fg = bgc;
          bg = selectedc;
          match.fg = fgc;
        };

        keyhint = {
          fg = fgc;
          suffix.fg = fgc;
          bg = bgc;
        };

        messages = {
          error.fg = bgc;
          error.bg = errorc;
          error.border = errorc;
          warning.fg = bgc;
          warning.bg = warningc;
          warning.border = warningc;
          info.fg = fgc;
          info.bg = bgc;
          info.border = bgc;
        };

        prompts = {
          fg = fgc;
          border = bgc;
          bg = bgc;
          selected.bg = selectedc;
        };

        statusbar = {
          normal.fg = statusc;
          normal.bg = bgc;
          insert.fg = fgc;
          insert.bg = bgc_alt;
          passthrough.fg = bgc;
          passthrough.bg = tabsindicatorc;
          private.fg = bgc;
          private.bg = bgc_alt;
          command.fg = fgc;
          command.bg = bgc;
          command.private.fg = fgc;
          command.private.bg = bgc;
          caret.fg = bgc;
          caret.bg = warningc;
          caret.selection.fg = bgc;
          caret.selection.bg = indicatorc;
          progress.bg = indicatorc;
          url.fg = fgc;
          url.error.fg = errorc;
          url.hover.fg = fgc;
          url.success.http.fg = warningc;
          url.success.https.fg = statusc;
          url.warn.fg = warningc;
        };

        tabs = {
          bar.bg = bgc;
          indicator.start = indicatorc;
          indicator.stop = tabsindicatorc;
          indicator.error = errorc;
          odd.fg = fgc;
          odd.bg = bgc;
          even.fg = fgc;
          even.bg = bgc;
          selected.odd.fg = bgc;
          selected.odd.bg = fgc;
          selected.even.fg = bgc;
          selected.even.bg = fgc;
        };
      };
    };
    searchEngines = {
      DEFAULT = "https://duckduckgo.org/{}";
      # META searchengines
      c2      = "https://search.carrot2.org/#/search/web/{}/treemap";
      y       = "https://www.yippy.com/search?query={}";
      # Alternative seachengines
      s       = "https://www.startpage.com/do/search?q={}";
      dd      = "https://duckduckgo.org/{}";
      g       = "https://google.com/search?q={}";
      # Tools
      du      = "https://www.duden.de/suchen/dudenonline/{}";
      cc      = "https://www.dict.cc/?s={}";
      dh      = "https://hub.docker.com/search?q={}&type=image";
      sh      = "https://sci-hub.st/{}";
      w       = "https://en.wikipedia.org/w/index.php?search={}";
      yt      = "https://youtube.com/results?search_query={}";
      aw      = "https://wiki.archlinux.org/index.php/{}";
      aur     = "https://aur.archlinux.org/packages/?K={}";
      doi     = "https://doi2bib.org/bib/{}";
      te      = "https://www.thesaurus.com/browse/{}";
      np      = "https://search.nixos.org/packages?channel=unstable&query={}";
      nw      = "https://nixos.wiki/index.php?search={}&go=Go";
      hm      = "https://github.com/nix-community/home-manager/search?q={}";
      rd      = "https://doc.rust-lang.org/std/index.html?search={}";
    };
    keyBindings = let leader = "<Space>"; in {
      normal = {
        "${leader}p" = "spawn --userscript qute-pass -U secret -u '^login: (.+)'";
        "${leader}y" = "spawn --userscript qute-pass --password-only";
        "${leader}Y" = "spawn --userscript qute-pass --username-only";
        "${leader}m" = "spawn sh -c '${pkgs.yt-dlp}/bin/yt-dlp {url} -o - | ${pkgs.mpv}/bin/mpv -'";
        "gP" = "tab-give --private";
        "gG" = "tab-give";
      };
      prompt = {
        "<Ctrl+h>" = "rl-filename-rubout";
      };
    };
  };

  home.packages = with pkgs; [
    # Access Netflix, Spotify, etc.
    vivaldi-widevine
  ];

  # Otherwise those are not writable
  xdg.configFile."qutebrowser/styles".source =
    config.lib.file.mkOutOfStoreSymlink "${dotf}/config/qutebrowser/styles";
  xdg.configFile."qutebrowser/scripts".source =
    config.lib.file.mkOutOfStoreSymlink "${dotf}/config/qutebrowser/scripts";
  xdg.configFile."qutebrowser/bookmarks".source =
    config.lib.file.mkOutOfStoreSymlink "${dotf}/config/qutebrowser/bookmarks";
  xdg.configFile."qutebrowser/quickmarks".source =
    config.lib.file.mkOutOfStoreSymlink "${dotf}/config/qutebrowser/quickmarks";
  xdg.configFile."qutebrowser/greasemonkey".source =
    config.lib.file.mkOutOfStoreSymlink "${dotf}/config/qutebrowser/greasemonkey";
}