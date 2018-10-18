"use strict";

var seedrandom = require('seedrandom');

exports.shuffle = function(seed) {
  return function(a1) {
    return function() {
      var rng = seedrandom(seed);
      var a = a1.slice();
      var j, x, i;
      for (i = a.length - 1; i > 0; i--) {
          j = Math.floor(rng() * (i + 1));
          x = a[i];
          a[i] = a[j];
          a[j] = x;
      }
      return a;
    };
  };
}
