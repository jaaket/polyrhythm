"use strict";

exports.requestAnimationFrame = function(cb) {
  return function(success, error) {
    window.requestAnimationFrame(function(time) {
      cb(time)(success, error);
    });
  }
}
