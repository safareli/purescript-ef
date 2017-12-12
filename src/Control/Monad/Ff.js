"use strict";


// Ef a 
// = { tag: "EFFECT", _0 :: () -> a,   _1 :: Void  }
// | { tag: "PURE",   _0 :: a,         _1 :: Void  }
// | { tag: "MAP",    _0 :: b -> a,    _1 :: Ef b }
// | { tag: "APPLY",  _0 :: Ef b,      _1 :: Ef (b -> a) }
// | { tag: "BIND",   _0 :: b -> Ef a, _1 :: Ef b }

// Operation a b
// = { tag: "MAP",        _0 :: a -> b }
// | { tag: "APPLY",      _0 :: Ef a }
// | { tag: "APPLY_FUNC", _0 :: a -> b }
// | { tag: "BIND",       _0 :: a -> Ef b }


function Ef(tag, _0, _1) {
  this.tag = tag;
  this._0 = _0;
  this._1 = _1;
}

var EFFECT = "EFFECT";
var PURE = "PURE";
var MAP = "MAP";
var APPLY = "APPLY";
var BIND = "BIND";
var APPLY_FUNC = "APPLY_FUNC";

exports.liftEffE = function (f) {
  return new Ef(EFFECT, f);
};

exports.pureE = function (x) {
  return new Ef(PURE, x);
};

exports.mapE = function (f) {
  return function (eff) {
    return new Ef(MAP, f, eff);
  };
};

exports.applyE = function (effF) {
  return function (eff) {
    return new Ef(APPLY, eff, effF);
  };
};

exports.bindE = function (eff) {
  return function (f) {
    return new Ef(BIND, f, eff);
  };
};

exports.toEff = function (inputEff) {
  return function() {
    var operations = [];
    var eff = inputEff;
    var res;
    var op;
    effLoop: for (;;) {
      if (eff.tag === MAP || eff.tag === BIND || eff.tag === APPLY) {
        operations.unshift(eff);
        eff = eff._1;
      } else {
        if (eff.tag === EFFECT) {
          res = eff._0();
        } else { // eff.tag === PURE
          res = eff._0;
        }
        while ((op = operations.shift())) {
          if (op.tag === MAP) {
            res = op._0(res);
          } else if (op.tag === APPLY_FUNC) {
            res = op._0(res);
          } else if (op.tag === APPLY) {
            eff = op._0;
            operations.unshift(new Ef(APPLY_FUNC, res));
            continue effLoop;
          } else { // op.tag === BIND
            eff = op._0(res);
            continue effLoop;
          }
        }
        return res;
      }
    }
  };
};