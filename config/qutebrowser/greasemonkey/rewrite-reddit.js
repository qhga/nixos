// ==UserScript==
// @name        rewrite-reddit
// @namespace   toerd
// @description Rewrite new to old reddit
// @include     *www.reddit.com*
// @version     1
// @run-at document-start
// ==/UserScript==

window.location.replace(window.location.toString().replace(/www/, 'old'));
