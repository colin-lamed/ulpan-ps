"use strict";

exports.setLocalStorage = function (k) {
  return function (v) {
    return function() {
      localStorage.setItem(k, v);
    };
  };
}

exports.jsGetLocalStorage = function (k) {
  return function() {
    return localStorage.getItem(k);
  };
}
