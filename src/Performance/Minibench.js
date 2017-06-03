"use strict";

exports.getTime = function() {
  return new Date().getTime();
};

exports.toExponential = function(n) {
  return n.toExponential(3);
};
