"use strict";

var ctx = new (window.AudioContext || window.webkitAudioContext)();

exports.play = function(data) {
  return function() {
    ctx.decodeAudioData(data, function(buffer) {
      var source = ctx.createBufferSource();
      source.connect(ctx.destination);
      source.buffer = buffer;
      source.start(0);
    });
  }
}
