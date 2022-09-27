# Load the autoconfig.yml file
config.load_autoconfig()

c.url.start_pages = ['https://duckduckgo.org/']
# c.url.start_pages = ['https://www.ecosia.org/?c=en']
c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.org/{}',
    # 'DEFAULT': 'https://www.ecosia.org/search?q={}',
    # META searchengines
    'c2': 'https://search.carrot2.org/#/search/web/{}/treemap',
    'y': 'https://www.yippy.com/search?query={}',
    # Alternative seachengines
    's': 'https://www.startpage.com/do/search?q={}',
    'dd': 'https://duckduckgo.org/{}',
    'g': 'https://google.com/search?q={}',
    # Tools
    'du': 'https://www.duden.de/suchen/dudenonline/{}',
    'cc': 'https://www.dict.cc/?s={}',
    'dh': 'https://hub.docker.com/search?q={}&type=image',
    'sh': 'https://sci-hub.st/{}',
    'w': 'https://en.wikipedia.org/w/index.php?search={}',
    'yt': 'https://youtube.com/results?search_query={}',
    'aw': 'https://wiki.archlinux.org/index.php/{}',
    'aur': 'https://aur.archlinux.org/packages/?K={}',
    'doi': 'https://doi2bib.org/bib/{}',
    'te': 'https://www.thesaurus.com/browse/{}'
}

# Bindings
leader = "<Space>"

config.bind(leader+'p', 'spawn --userscript qute-pass -U secret -u "^login: (.+)"')
config.bind(leader+'y', 'spawn --userscript qute-pass --password-only')
# DEPENDS: pip install simplejson pynacl
# key: gpg --card-status (encrytion key rsa4096/XXXX)
# config.bind(leader+'p', 'spawn --userscript ~/.config/qutebrowser/scripts/qute-keepassxc --key FC3BF722E2EF4C89')
# config.bind(leader+'ba', 'spawn --userscript ~/.config/qutebrowser/scripts/qute-capture write')
# config.bind(leader+'bb', 'spawn --userscript ~/.config/qutebrowser/scripts/qute-capture read')
# config.bind(leader+'bd', 'spawn --userscript ~/.config/qutebrowser/scripts/qute-capture rm')
config.bind(leader+'m', 'spawn mpv {url}')
config.bind(leader+'g', 'greasemonkey-reload')
config.bind('yr', 'spawn --userscript ~/.config/qutebrowser/scripts/get_yt_rss_feed')
config.bind('gP', 'tab-give --private')
config.bind('gG', 'tab-give')
# config.bind('yR', 'jseval --world main console.log(ytInitialData.responseContext.serviceTrackingParams[0].params[0].value)')

c.content.blocking.method = 'both'

# toggle stylesheet
# c.content.user_stylesheets = ["~/.config/qutebrowser/styles/floppy.css"]
c.content.user_stylesheets = []
# config.bind(leader+'td', 'set content.user_stylesheets ~/.config/qutebrowser/styles/dark.css')
# config.bind(leader+'td', 'set content.user_stylesheets ~/.config/qutebrowser/styles/floppy.css')
# config.bind(leader+'ta', 'set content.user_stylesheets "[~/.config/qutebrowser/styles/solarized.css, ~/.config/qutebrowser/styles/floppy.css]"')

# editor
c.editor.command = ["emacs", "{}", "{file}", "{}"]

# downloads
c.downloads.remove_finished = 5000
config.bind('<Ctrl+h>', 'rl-filename-rubout', mode='prompt')

# content
# c.content.cache.size = None
# c.content.cookies.accept = 'no-3rdparty' # Valid values: all, no-3rdparty, no-unknown-3rdparty, never
# c.content.user_stylesheets = ["~/.config/qutebrowser/styles/floppy.css"]

c.content.desktop_capture = 'ask' #'ask'
c.content.javascript.alert = False
c.content.local_storage = True # WOW REDDIT AND OTHER WEBSITES
c.content.cookies.store = True

c.content.default_encoding = 'utf-8'
c.content.geolocation = 'ask' # Valid values: true, false, ask
c.content.headers.accept_language = 'en-US,en,de'
c.content.headers.custom = {}
c.content.headers.do_not_track = True
c.content.headers.referer = 'same-domain' # Valid values: always, never, same-domain
# c.content.headers.user_agent = 'Windows-Media-Player/11.0.5721.5145'
# c.content.headers.user_agent = 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:77.0) Gecko/20100101 Firefox/77.0'
c.content.headers.user_agent = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36'

#c.content.blocking.enabled = False
# # Beide funken... nur halt nicht fuer hostblocking...
# # config.set('content.host_blocking.enabled', False, '*://reddit.org/*')
# # with config.pattern('*://github.org/') as p:
# #     p.content.host_blocking.enabled = False

c.content.blocking.hosts.lists = ['https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts']
c.content.blocking.whitelist = ['piwik.org', '*.reddit.com', '*.redditinc.com', '*.redditstatic.com']

font = "15px Ttyp0"
# fonts
c.fonts.tabs.selected = font
c.fonts.tabs.unselected = font
c.fonts.statusbar = font
c.fonts.downloads = c.fonts.statusbar
c.fonts.prompts = c.fonts.statusbar
c.fonts.hints = font
c.fonts.messages.info = font
c.fonts.keyhint = c.fonts.messages.info
c.fonts.messages.warning = c.fonts.messages.info
c.fonts.messages.error = c.fonts.messages.info
c.fonts.completion.entry = c.fonts.statusbar
c.fonts.completion.category = c.fonts.statusbar

c.zoom.default = '100%'

c.tabs.padding = {"top": 0, "bottom": 0, "left": 5, "right": 5}
c.tabs.favicons.scale = 0.8

c.statusbar.padding = {"top": 0, "bottom": 0, "left": 5, "right": 5}

bgc = "#0d1f2d"
bgc_alt = "#1D2F3D"
fgc = "#c3c9e9"

errorc = "#FFC0CB"
warningc = "#FF8C00"
selectedc = "#a2dfed"
statusc = "#a5cc69"
tabsindicatorc = "#fefefe"
indicatorc = "#fefefe"
# set qutebrowser colors

# Text color of the completion widget. May be a single color to use for
# all columns or a list of three colors, one for each column.
c.colors.completion.fg = fgc

# Background color of the completion widget for odd rows.
c.colors.completion.odd.bg = bgc

# Background color of the completion widget for even rows.
c.colors.completion.even.bg = bgc

# Foreground color of completion widget category headers.
c.colors.completion.category.fg = selectedc

# Background color of the completion widget category headers.
c.colors.completion.category.bg = bgc

# Top border color of the completion widget category headers.
c.colors.completion.category.border.top = bgc

# Bottom border color of the completion widget category headers.
c.colors.completion.category.border.bottom = bgc

# Foreground color of the selected completion item.
c.colors.completion.item.selected.fg = bgc

# Background color of the selected completion item.
c.colors.completion.item.selected.bg = selectedc

# Top border color of the completion widget category headers.
c.colors.completion.item.selected.border.top = selectedc

# Bottom border color of the selected completion item.
c.colors.completion.item.selected.border.bottom = selectedc

# Foreground color of the matched text in the completion.
c.colors.completion.match.fg = selectedc

# Color of the scrollbar handle in the completion view.
c.colors.completion.scrollbar.fg = fgc

# Color of the scrollbar in the completion view.
c.colors.completion.scrollbar.bg = bgc

# Background color for the download bar.
c.colors.downloads.bar.bg = bgc

# Color gradient start for download text.
c.colors.downloads.start.fg = bgc

# Color gradient start for download backgrounds.
c.colors.downloads.start.bg = selectedc

# Color gradient end for download text.
c.colors.downloads.stop.fg = bgc

# Color gradient stop for download backgrounds.
c.colors.downloads.stop.bg = statusc

# Foreground color for downloads with errors.
c.colors.downloads.error.fg = errorc

# Font color for hints.
c.colors.hints.fg = bgc

# Background color for hints. Note that you can use a `rgba(...)` value
# for transparency.
c.colors.hints.bg = selectedc

# Font color for the matched part of hints.
c.colors.hints.match.fg = fgc

# Text color for the keyhint widget.
c.colors.keyhint.fg = fgc

# Highlight color for keys to complete the current keychain.
c.colors.keyhint.suffix.fg = fgc

# Background color of the keyhint widget.
c.colors.keyhint.bg = bgc

# Foreground color of an error message.
c.colors.messages.error.fg = bgc

# Background color of an error message.
c.colors.messages.error.bg = errorc

# Border color of an error message.
c.colors.messages.error.border = errorc

# Foreground color of a warning message.
c.colors.messages.warning.fg = bgc

# Background color of a warning message.
c.colors.messages.warning.bg = warningc

# Border color of a warning message.
c.colors.messages.warning.border = warningc

# Foreground color of an info message.
c.colors.messages.info.fg = fgc

# Background color of an info message.
c.colors.messages.info.bg = bgc

# Border color of an info message.
c.colors.messages.info.border = bgc

# Foreground color for prompts.
c.colors.prompts.fg = fgc

# Border used around UI elements in prompts.
c.colors.prompts.border = bgc

# Background color for prompts.
c.colors.prompts.bg = bgc

# Background color for the selected item in filename prompts.
c.colors.prompts.selected.bg = selectedc

# Foreground color of the statusbar.
c.colors.statusbar.normal.fg = statusc

# Background color of the statusbar.
c.colors.statusbar.normal.bg = bgc

# Foreground color of the statusbar in insert mode.
c.colors.statusbar.insert.fg = fgc

# Background color of the statusbar in insert mode.
c.colors.statusbar.insert.bg = bgc_alt

# Foreground color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.fg = bgc

# Background color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.bg = tabsindicatorc

# Foreground color of the statusbar in private browsing mode.
c.colors.statusbar.private.fg = bgc

# Background color of the statusbar in private browsing mode.
c.colors.statusbar.private.bg = bgc_alt

# Foreground color of the statusbar in command mode.
c.colors.statusbar.command.fg = fgc

# Background color of the statusbar in command mode.
c.colors.statusbar.command.bg = bgc

# Foreground color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.fg = fgc

# Background color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.bg = bgc

# Foreground color of the statusbar in caret mode.
c.colors.statusbar.caret.fg = bgc

# Background color of the statusbar in caret mode.
c.colors.statusbar.caret.bg = warningc

# Foreground color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.fg = bgc

# Background color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.bg = indicatorc

# Background color of the progress bar.
c.colors.statusbar.progress.bg = indicatorc

# Default foreground color of the URL in the statusbar.
c.colors.statusbar.url.fg = fgc

# Foreground color of the URL in the statusbar on error.
c.colors.statusbar.url.error.fg = errorc

# Foreground color of the URL in the statusbar for hovered links.
c.colors.statusbar.url.hover.fg = fgc

# Foreground color of the URL in the statusbar on successful load
# (http).
c.colors.statusbar.url.success.http.fg = warningc

# Foreground color of the URL in the statusbar on successful load
# (https).
c.colors.statusbar.url.success.https.fg = statusc

# Foreground color of the URL in the statusbar when there's a warning.
c.colors.statusbar.url.warn.fg = warningc

# Background color of the tab bar.
c.colors.tabs.bar.bg = bgc

# Color gradient start for the tab indicator.
c.colors.tabs.indicator.start = indicatorc

# Color gradient end for the tab indicator.
c.colors.tabs.indicator.stop = tabsindicatorc

# Color for the tab indicator on errors.
c.colors.tabs.indicator.error = errorc

# Foreground color of unselected odd tabs.
c.colors.tabs.odd.fg = fgc

# Background color of unselected odd tabs.
c.colors.tabs.odd.bg = bgc

# Foreground color of unselected even tabs.
c.colors.tabs.even.fg = fgc

# Background color of unselected even tabs.
c.colors.tabs.even.bg = bgc

# Foreground color of selected odd tabs.
c.colors.tabs.selected.odd.fg = bgc

# Background color of selected odd tabs.
c.colors.tabs.selected.odd.bg = fgc

# Foreground color of selected even tabs.
c.colors.tabs.selected.even.fg = bgc

# Background color of selected even tabs.
c.colors.tabs.selected.even.bg = fgc


#c.backend = 'webkit' # Valid values: webengine, webkit
