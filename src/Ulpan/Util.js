"use strict";

var $ = require("jquery");

// for `hideModal`. Also required to keep bootstrap when bundling as a single js file.
var Bootstrap = require("bootstrap");

exports.shuffle = function(a1) {
  return function() {
    var a = a1.slice();
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
  };
}
