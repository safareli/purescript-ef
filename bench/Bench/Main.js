"use strict";

exports.mkArr = function(){
  return [];
};

exports.pushToArr = function(xs) {
  return function(x) {
    return function() {
      xs.push(x);
      return x;
    };
  };
};

exports.log = function(x) {
  return function(){
    console.log(x)
  }
};