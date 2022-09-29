{ config, pkgs, user, dotf, ... }:
let
  t = (import ../../themes).shanty;
in
{

  programs.qutebrowser = {
    enable = true;
    settings = {
      qt.args = [ "widevine-path=${pkgs.vivaldi-widevine}/share/google/chrome/WidevineCdm/_platform_specific/linux_x64/libwidevinecdm.so" ];
      url = {
        start_pages = [ "https://duckduckgo.org/" ];
      };
      fonts = let font = "15px Ttyp0"; in {
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
        blocking.method = "both";
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
        blocking.hosts.lists = ["https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"];
        blocking.whitelist = ["piwik.org" "*.reddit.com" "*.redditinc.com" "*.redditstatic.com"];
      };
      colors = let
        bgc = t.background;
        bgc_alt = t.background_alt;
        fgc = t.foreground;
        errorc = t.error;
        warningc = t.warning;
        selectedc = t.cyan;
        statusc = t.green;
        tabsindicatorc = t.white;
        indicatorc = t.white;
      in {
        completion.fg = fgc;
        completion.odd.bg = bgc;
        completion.even.bg = bgc;
        completion.category.fg = selectedc;
        completion.category.bg = bgc;
        completion.category.border.top = bgc;
        completion.category.border.bottom = bgc;
        completion.item.selected.fg = bgc;
        completion.item.selected.bg = selectedc;
        completion.item.selected.border.top = selectedc;
        completion.item.selected.border.bottom = selectedc;
        completion.match.fg = selectedc;
        completion.scrollbar.fg = fgc;
        completion.scrollbar.bg = bgc;
        downloads.bar.bg = bgc;
        downloads.start.fg = bgc;
        downloads.start.bg = selectedc;
        downloads.stop.fg = bgc;
        downloads.stop.bg = statusc;
        downloads.error.fg = errorc;
        hints.fg = bgc;
        hints.bg = selectedc;
        hints.match.fg = fgc;
        keyhint.fg = fgc;
        keyhint.suffix.fg = fgc;
        keyhint.bg = bgc;
        messages.error.fg = bgc;
        messages.error.bg = errorc;
        messages.error.border = errorc;
        messages.warning.fg = bgc;
        messages.warning.bg = warningc;
        messages.warning.border = warningc;
        messages.info.fg = fgc;
        messages.info.bg = bgc;
        messages.info.border = bgc;
        prompts.fg = fgc;
        prompts.border = bgc;
        prompts.bg = bgc;
        prompts.selected.bg = selectedc;
        statusbar.normal.fg = statusc;
        statusbar.normal.bg = bgc;
        statusbar.insert.fg = fgc;
        statusbar.insert.bg = bgc_alt;
        statusbar.passthrough.fg = bgc;
        statusbar.passthrough.bg = tabsindicatorc;
        statusbar.private.fg = bgc;
        statusbar.private.bg = bgc_alt;
        statusbar.command.fg = fgc;
        statusbar.command.bg = bgc;
        statusbar.command.private.fg = fgc;
        statusbar.command.private.bg = bgc;
        statusbar.caret.fg = bgc;
        statusbar.caret.bg = warningc;
        statusbar.caret.selection.fg = bgc;
        statusbar.caret.selection.bg = indicatorc;
        statusbar.progress.bg = indicatorc;
        statusbar.url.fg = fgc;
        statusbar.url.error.fg = errorc;
        statusbar.url.hover.fg = fgc;
        statusbar.url.success.http.fg = warningc;
        statusbar.url.success.https.fg = statusc;
        statusbar.url.warn.fg = warningc;
        tabs.bar.bg = bgc;
        tabs.indicator.start = indicatorc;
        tabs.indicator.stop = tabsindicatorc;
        tabs.indicator.error = errorc;
        tabs.odd.fg = fgc;
        tabs.odd.bg = bgc;
        tabs.even.fg = fgc;
        tabs.even.bg = bgc;
        tabs.selected.odd.fg = bgc;
        tabs.selected.odd.bg = fgc;
        tabs.selected.even.fg = bgc;
        tabs.selected.even.bg = fgc;
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
    };
    keyBindings = let leader = "<Space>"; in {
      normal = {
        "${leader}p" = "spawn --userscript qute-pass -U secret -u '^login: (.+)'";
        "${leader}y" = "spawn --userscript qute-pass --password-only";
        "${leader}Y" = "spawn --userscript qute-pass --username-only";
        "${leader}m" = "spawn ${pkgs.mpv}/bin/mpv {url}";
        "gP" = "tab-give --private";
        "gG" = "tab-give";
      };
    };
  };

  home.packages = with pkgs; [
    # Access Netflix, Spotify, etc.
    vivaldi-widevine
  ];
  xdg.configFile."qutebrowser/styles".source = ../../../config/qutebrowser/styles;
  xdg.configFile."qutebrowser/scripts".source = ../../../config/qutebrowser/scripts;
  xdg.configFile."qutebrowser/bookmarks".source = ../../../config/qutebrowser/bookmarks;
  xdg.configFile."qutebrowser/quickmarks".source = ../../../config/qutebrowser/quickmarks;
  xdg.configFile."qutebrowser/greasemonkey".source = ../../../config/qutebrowser/greasemonkey;
}