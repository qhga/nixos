// ==UserScript==
// @name        stack-date-time
// @namespace   phga
// @description Display "normal" timestamps on SE Answers
// @include     *stack*
// @version     1
// @run-at      document-load
// ==/UserScript==
(function() {
    const reltimes = [...document.getElementsByClassName("relativetime")];
    console.log(reltimes);
    reltimes.forEach(e => {
        e.innerHTML = e.title;
    });
})();
