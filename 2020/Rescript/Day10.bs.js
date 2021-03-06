// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_Int = require("bs-platform/lib/js/belt_Int.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_splice_call = require("bs-platform/lib/js/caml_splice_call.js");

function findChoicesForAllAdapter(adapters) {
  var _choices = /* [] */0;
  var _remain = adapters;
  while(true) {
    var remain = _remain;
    var choices = _choices;
    if (remain.length === 0) {
      return choices;
    }
    var min = Caml_splice_call.spliceApply(Math.min, [remain]);
    var remaining = Belt_Array.keep(remain, (function(min){
        return function (x) {
          return x !== min;
        }
        }(min)));
    var nextMin = Caml_splice_call.spliceApply(Math.min, [remaining]);
    var nextChoices = Belt_List.add(choices, {
          adapter: min,
          diff: nextMin - min | 0
        });
    _remain = remaining;
    _choices = nextChoices;
    continue ;
  };
}

function addForChargingOutlet(adapters) {
  return Belt_Array.concat(adapters, [0]);
}

function addBuiltinAdapter(adapters) {
  var max = Caml_splice_call.spliceApply(Math.max, [adapters]);
  return Belt_Array.concat(adapters, [max + 3 | 0]);
}

function countDiff(c) {
  var a = Belt_List.length(Belt_List.keep(c, (function (param) {
              return param.diff === 1;
            })));
  var b = Belt_List.length(Belt_List.keep(c, (function (param) {
              return param.diff === 3;
            })));
  return Math.imul(a, b);
}

var adapters = Belt_Array.keepMap(Fs.readFileSync("res/day10-sample-1.txt", "utf8").split("\n"), Belt_Int.fromString);

var adapters2 = Belt_Array.keepMap(Fs.readFileSync("res/day10-sample-2.txt", "utf8").split("\n"), Belt_Int.fromString);

var adapters$1 = Belt_Array.keepMap(Fs.readFileSync("res/day10.txt", "utf8").split("\n"), Belt_Int.fromString);

console.log(countDiff(findChoicesForAllAdapter(addBuiltinAdapter(Belt_Array.concat(adapters$1, [0])))));

function combine(a, b, condition) {
  return Belt_Array.reduce(a, /* [] */0, (function (l, a0) {
                return Belt_Array.reduce(b, l, (function (l, b0) {
                              if (Curry._2(condition, a0, b0)) {
                                return Belt_List.add(l, [
                                            a0,
                                            b0
                                          ]);
                              } else {
                                return l;
                              }
                            }));
              }));
}

function printList(l) {
  console.log(Belt_Array.map(Belt_List.toArray(l), Belt_List.toArray));
  return l;
}

function spliceBy3(adapters) {
  var diff3 = function (_res, _curr, _l) {
    while(true) {
      var l = _l;
      var curr = _curr;
      var res = _res;
      if (!l) {
        return ;
      }
      var b = l.tl;
      var a = l.hd;
      if (b) {
        var b$1 = b.hd;
        if ((b$1 - a | 0) === 3) {
          _l = {
            hd: b$1,
            tl: b.tl
          };
          _curr = /* [] */0;
          _res = Belt_List.add(res, Belt_List.add(curr, a));
          continue ;
        }
        
      }
      if (b === /* [] */0) {
        return Belt_List.add(res, Belt_List.add(curr, a));
      }
      _l = b;
      _curr = Belt_List.add(curr, a);
      continue ;
    };
  };
  return Belt_List.map(Belt_Option.getExn(diff3(/* [] */0, /* [] */0, adapters)), Belt_List.reverse);
}

function countArrangement(adapterList) {
  var countNext = function (_remain) {
    while(true) {
      var remain = _remain;
      if (!remain) {
        return 1;
      }
      var match = remain.tl;
      if (!match) {
        return 1;
      }
      var c = match.tl;
      var b = match.hd;
      var a = remain.hd;
      if (c) {
        var d = c.tl;
        var c$1 = c.hd;
        var exit = 0;
        if (d) {
          var d$1 = d.hd;
          if ((d$1 - a | 0) === 3) {
            var e = d.tl;
            return countNext({
                        hd: d$1,
                        tl: e
                      }) + countNext({
                        hd: c$1,
                        tl: {
                          hd: d$1,
                          tl: e
                        }
                      }) + countNext({
                        hd: b,
                        tl: {
                          hd: c$1,
                          tl: {
                            hd: d$1,
                            tl: e
                          }
                        }
                      });
          }
          exit = 2;
        } else {
          exit = 2;
        }
        if (exit === 2 && (c$1 - a | 0) <= 3) {
          return countNext({
                      hd: c$1,
                      tl: d
                    }) + countNext({
                      hd: b,
                      tl: {
                        hd: c$1,
                        tl: d
                      }
                    });
        }
        
      }
      if ((b - a | 0) > 2) {
        return 1;
      }
      _remain = {
        hd: b,
        tl: c
      };
      continue ;
    };
  };
  console.log(countNext({
            hd: 4,
            tl: {
              hd: 5,
              tl: {
                hd: 6,
                tl: {
                  hd: 7,
                  tl: /* [] */0
                }
              }
            }
          }));
  var x = Belt_List.map(printList(spliceBy3(adapterList)), countNext);
  return Belt_List.reduce((console.log(Belt_List.toArray(x)), x), 1, (function (a, b) {
                return a * b;
              }));
}

var adapters$2 = Belt_Array.keepMap(Fs.readFileSync("res/day10.txt", "utf8").split("\n"), Belt_Int.fromString);

console.log(countArrangement(Belt_List.fromArray(addBuiltinAdapter(Belt_Array.concat(adapters$2, [0])).sort(function (n1, n2) {
                  return n1 - n2 | 0;
                }))));

var p1;

exports.findChoicesForAllAdapter = findChoicesForAllAdapter;
exports.addForChargingOutlet = addForChargingOutlet;
exports.addBuiltinAdapter = addBuiltinAdapter;
exports.countDiff = countDiff;
exports.adapters = adapters;
exports.adapters2 = adapters2;
exports.p1 = p1;
exports.combine = combine;
exports.printList = printList;
exports.spliceBy3 = spliceBy3;
exports.countArrangement = countArrangement;
/* adapters Not a pure module */
