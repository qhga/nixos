// ==UserScript==
// @name        no-cookies-for-qute
// @namespace   toerd
// @description Get rid of cookie banners
// @include     *
// @version     1
// @run-at document-load
// ==/UserScript==

// consider server side: https://github.com/dgraph-io/badger
// maybe just use a map and write to file (json) on update.
// load file on restart/start of server, share files
// get(host) -> element[]
// add(host, element[]) -> success
// serverside: add() check for existing elements that are the same? maybe not
// context menu on client side to pick parent element interactively
document.addEventListener('contextmenu', (e) => {
    if (!e.altKey)
        return false;

    e.preventDefault();
    hideCookieBanner(e.target);
    return false;
});

function hideCookieBanner(banner) {
    banner.style.display = 'none';
    msg("Cookie hidden");
}

async function msg(text) {
    let m = document.createElement("div");
    m.innerHTML = text;
    m.style.position = 'fixed';
    m.style.right = 0;
    m.style.top = 0;
    m.style.backgroundColor = '#333';
    m.style.color = '#fefefe';
    m.style.zIndex = '69';
    document.body.appendChild(m);
    removeMsg(m);
}

function removeMsg(m) {
    setTimeout(() => {
        document.body.removeChild(m);
    }, 2000);
}
