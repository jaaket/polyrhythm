"use strict";

var ctx = new (window.AudioContext || window.webkitAudioContext)();
var samples = [];

exports.loadSample_ = function(cb) {
  return function(data) {
    return function() {
      ctx.decodeAudioData(data, function(buffer) {
        samples.push(buffer);
        // cb(foo) returns Eff ? ?, represented as a function with no arguments
        // which when applied performs the wanted effects.
        cb(samples.length - 1)();
      });
    }
  }
}

exports.play = function(sampleIdx) {
  return function() {
    var source = ctx.createBufferSource();
    source.connect(ctx.destination);
    source.buffer = samples[sampleIdx];
    source.start(0);
  }
}
