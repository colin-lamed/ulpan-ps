"use strict";


exports._swipe = function(settings) {
  return function (elm) {
    return function (onSwipe) {
      return function() {
        var stillMoving = false;
        var start;

        elm.addEventListener('touchstart', onTouchStart, false);

        function onTouchStart(e) {
          if (e.touches.length == 1) {
            start = e.touches[0].pageX;
            stillMoving = true;
            elm.addEventListener('touchmove', onTouchMove, false);
          }
        };

        function onTouchMove(e) {
          if (stillMoving) {
            var x = e.touches[0].pageX;
            var difference = start - x;
            if (Math.abs(difference) >= settings.threshold) {
              cancelTouch();
              if (difference > 0) {
                onSwipe(true);
              }
              else {
                onSwipe(false);
              }
            }
          }
        };

        function cancelTouch() {
          elm.removeEventListener('touchmove', onTouchMove);
          start = null;
          stillMoving = false;
        };
      }
    };
  };
};
