"use strict";

// module Pure.UI.Thermite.Factorial

exports.getInt = function(e) {
  console.log(e.target.value);
  return parseInt(e.target.value);
};

exports.getValue = function(e) {
  console.log(e.target.value);
  return e.target.value;
};

exports.getKeyCode = function(e) {
  console.log('wtf', e.keyCode);
  return e.keyCode;
};
