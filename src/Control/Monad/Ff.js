"use strict";


// Ef a
// = () -> a
// | { Ef a, tag: "PURE",   _0 :: a,         _1 :: Void  }
// | { Ef a, tag: "MAP",    _0 :: b -> a,    _1 :: Ef b }
// | { Ef a, tag: "APPLY",  _0 :: Ef b,      _1 :: Ef (b -> a) }
// | { Ef a, tag: "BIND",   _0 :: b -> Ef a, _1 :: Ef b }

// Operation a b
// = { tag: "MAP",        _0 :: a -> b }
// | { tag: "APPLY",      _0 :: Ef a }
// | { tag: "APPLY_FUNC", _0 :: a -> b }
// | { tag: "BIND",       _0 :: a -> Ef b }


var PURE = "PURE";
var MAP = "MAP";
var APPLY = "APPLY";
var BIND = "BIND";
var APPLY_FUNC = "APPLY_FUNC";

exports.pureE = function (x) {
  return mkEf(PURE, x);
};

exports.mapE = function (f) {
  return function (eff) {
    return mkEf(MAP, f, eff);
  };
};

exports.applyE = function (effF) {
  return function (eff) {
    return mkEf(APPLY, eff, effF);
  };
};

exports.bindE = function (eff) {
  return function (f) {
    return mkEf(BIND, f, eff);
  };
};


var mkEf = function (tag, _0, _1) {
  var eff = function eff_() { return toEff(eff_) }
  eff.tag = tag
  eff._0 = _0
  eff._1 = _1
  return eff
}

var toEff = function (inputEff) {
  var operations = [];
  var eff = inputEff;
  var res;
  var op;
  var tag;
  effLoop: for (;;) {
    tag = eff.tag;
    if (tag !== undefined) {
      if (tag === MAP || tag === BIND || tag === APPLY) {
        operations.push(eff);
        eff = eff._1;
        continue;
      }
      // here `tag === PURE`
      res = eff._0;
    } else {
      // here `typeof eff == "function"`
      res = eff();
    }

    while ((op = operations.pop())) {
      if (op.tag === MAP) {
        res = op._0(res);
      } else if (op.tag === APPLY_FUNC) {
        res = op._0(res);
      } else if (op.tag === APPLY) {
        eff = op._0;
        operations.push(new Ef(APPLY_FUNC, res));
        continue effLoop;
      } else { // op.tag === BIND
        eff = op._0(res);
        continue effLoop;
      }
    }
    return res;
  }
};