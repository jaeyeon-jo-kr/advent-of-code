// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_Int = require("bs-platform/lib/js/belt_Int.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

function identity(x) {
  return x;
}

function Quarantine(Finder) {
  var isMatchesNext = function (capcha, currentIndex) {
    return Caml_array.get(capcha, currentIndex) === Caml_array.get(capcha, Curry._2(Finder.nextIndex, capcha, currentIndex));
  };
  var matchedValue = function (capcha, currentIndex, digit) {
    if (isMatchesNext(capcha, currentIndex)) {
      return Caml_option.some(digit);
    }
    
  };
  var matchedValues = function (capcha) {
    return Belt_Array.keepMap(Belt_Array.mapWithIndex(capcha, (function (param, param$1) {
                      return matchedValue(capcha, param, param$1);
                    })), identity);
  };
  var sumOfValues = function (values) {
    return Belt_Array.reduce(values, 0, (function (s, i) {
                  return s + i | 0;
                }));
  };
  var inputToCapcha = function (input) {
    return Belt_Array.keepMap(Belt_Array.map(input.split(""), Belt_Int.fromString), identity);
  };
  var capchaInputs = Fs.readFileSync("./res/day2017-1.txt", "utf8");
  var solution = function (param) {
    console.log(sumOfValues(matchedValues(inputToCapcha(capchaInputs))));
    
  };
  return {
          isMatchesNext: isMatchesNext,
          matchedValue: matchedValue,
          matchedValues: matchedValues,
          sumOfValues: sumOfValues,
          inputToCapcha: inputToCapcha,
          capchaInputs: capchaInputs,
          solution: solution
        };
}

function nextIndex(capcha, current) {
  return Caml_int32.mod_(current + 1 | 0, capcha.length);
}

var P1 = {
  nextIndex: nextIndex
};

function isMatchesNext(capcha, currentIndex) {
  return Caml_array.get(capcha, currentIndex) === Caml_array.get(capcha, nextIndex(capcha, currentIndex));
}

function matchedValue(capcha, currentIndex, digit) {
  if (isMatchesNext(capcha, currentIndex)) {
    return Caml_option.some(digit);
  }
  
}

function matchedValues(capcha) {
  return Belt_Array.keepMap(Belt_Array.mapWithIndex(capcha, (function (param, param$1) {
                    return matchedValue(capcha, param, param$1);
                  })), identity);
}

function sumOfValues(values) {
  return Belt_Array.reduce(values, 0, (function (s, i) {
                return s + i | 0;
              }));
}

function inputToCapcha(input) {
  return Belt_Array.keepMap(Belt_Array.map(input.split(""), Belt_Int.fromString), identity);
}

var capchaInputs = Fs.readFileSync("./res/day2017-1.txt", "utf8");

function solution(param) {
  console.log(sumOfValues(matchedValues(inputToCapcha(capchaInputs))));
  
}

var P1Solution = {
  isMatchesNext: isMatchesNext,
  matchedValue: matchedValue,
  matchedValues: matchedValues,
  sumOfValues: sumOfValues,
  inputToCapcha: inputToCapcha,
  capchaInputs: capchaInputs,
  solution: solution
};

solution(undefined);

function nextIndex$1(capcha, current) {
  var l = capcha.length;
  var diff = l / 2 | 0;
  return Caml_int32.mod_(current + diff | 0, l);
}

var P2 = {
  nextIndex: nextIndex$1
};

function isMatchesNext$1(capcha, currentIndex) {
  return Caml_array.get(capcha, currentIndex) === Caml_array.get(capcha, nextIndex$1(capcha, currentIndex));
}

function matchedValue$1(capcha, currentIndex, digit) {
  if (isMatchesNext$1(capcha, currentIndex)) {
    return Caml_option.some(digit);
  }
  
}

function matchedValues$1(capcha) {
  return Belt_Array.keepMap(Belt_Array.mapWithIndex(capcha, (function (param, param$1) {
                    return matchedValue$1(capcha, param, param$1);
                  })), identity);
}

function sumOfValues$1(values) {
  return Belt_Array.reduce(values, 0, (function (s, i) {
                return s + i | 0;
              }));
}

function inputToCapcha$1(input) {
  return Belt_Array.keepMap(Belt_Array.map(input.split(""), Belt_Int.fromString), identity);
}

var capchaInputs$1 = Fs.readFileSync("./res/day2017-1.txt", "utf8");

function solution$1(param) {
  console.log(sumOfValues$1(matchedValues$1(inputToCapcha$1(capchaInputs$1))));
  
}

var P2Solution = {
  isMatchesNext: isMatchesNext$1,
  matchedValue: matchedValue$1,
  matchedValues: matchedValues$1,
  sumOfValues: sumOfValues$1,
  inputToCapcha: inputToCapcha$1,
  capchaInputs: capchaInputs$1,
  solution: solution$1
};

solution$1(undefined);

exports.identity = identity;
exports.Quarantine = Quarantine;
exports.P1 = P1;
exports.P1Solution = P1Solution;
exports.P2 = P2;
exports.P2Solution = P2Solution;
/* capchaInputs Not a pure module */
