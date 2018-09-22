"use strict";

exports._registerServiceWorker = function(serviceWorkerJs) {
  return function(onError, onSuccess) {
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker
        .register(serviceWorkerJs)
        .then(onSuccess)
        .catch(onError);
    } else {
      onError("No serviceWorker");
    }
  };
};
