"use strict";

exports.initContext = function(foo) {
  console.log("foo");
  return new (window.AudioContext || window.webkitAudioContext)();
}

exports.play = function(ctx) {
  return function(data) {
    ctx.decodeAudioData(data, function(buffer) {
      var source = ctx.createBufferSource();
      source.connect(ctx.destination);
      source.buffer = buffer;
      source.start(0);
    });
  }
}
