"use strict";

exports.registerServiceWorker = function(serviceWorkerJs) {
  return function() {
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker
        .register(serviceWorkerJs)
        .then(function() { console.log('Service Worker Registered'); });
    }
  };
};
