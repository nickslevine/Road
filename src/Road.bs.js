// Generated by BUCKLESCRIPT VERSION 4.0.8, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");

function reverse(str) {
  var len = str.length;
  return $$String.init(len, (function (i) {
                return Caml_string.get(str, (len - 1 | 0) - i | 0);
              }));
}

var $$String$1 = /* module */[/* reverse */reverse];

function max(lst) {
  return List.fold_left((function (acc, x) {
                if (acc > x) {
                  return acc;
                } else {
                  return x;
                }
              }), Number.NEGATIVE_INFINITY, lst);
}

function min(lst) {
  return List.fold_left((function (acc, x) {
                if (acc < x) {
                  return acc;
                } else {
                  return x;
                }
              }), Number.POSITIVE_INFINITY, lst);
}

function sum(lst) {
  return List.fold_left((function (acc, x) {
                return acc + x | 0;
              }), 0, lst);
}

function product(lst) {
  return List.fold_left(Caml_int32.imul, 1, lst);
}

function range(start, end_) {
  if (start >= end_) {
    return /* [] */0;
  } else {
    return /* :: */[
            start,
            range(start + 1 | 0, end_)
          ];
  }
}

var List$1 = /* module */[
  /* max */max,
  /* min */min,
  /* sum */sum,
  /* product */product,
  /* range */range
];

exports.$$String = $$String$1;
exports.List = List$1;
/* No side effect */
