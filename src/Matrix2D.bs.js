// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Float = require("./Float.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function makeUninitializedUnsafe(param) {
  var matrix = new Array(2);
  matrix[0] = new Array(2);
  matrix[1] = new Array(2);
  return matrix;
}

function getUnsafe(t, row, col) {
  return t[row][col];
}

function setUnsafe(t, x, row, col) {
  t[row][col] = x;
  return /* () */0;
}

var identityMatrix = /* array */[
  /* array */[
    1,
    0
  ],
  /* array */[
    0,
    1
  ]
];

function equals(matrix1, matrix2) {
  var _i = 0;
  var _j = 0;
  while(true) {
    var j = _j;
    var i = _i;
    if (i > 1) {
      return true;
    } else {
      var x = getUnsafe(matrix1, i, j);
      var y = getUnsafe(matrix2, i, j);
      if (Float.equals(x, y)) {
        if (j < 1) {
          _j = j + 1 | 0;
          continue ;
        } else {
          _j = 0;
          _i = i + 1 | 0;
          continue ;
        }
      } else {
        return false;
      }
    }
  };
}

function multiply(m1, m2) {
  var m3 = makeUninitializedUnsafe(/* () */0);
  for(var row = 0; row <= 1; ++row){
    for(var col = 0; col <= 1; ++col){
      setUnsafe(m3, getUnsafe(m1, row, 0) * getUnsafe(m2, 0, col) + getUnsafe(m1, row, 1) * getUnsafe(m2, 1, col), row, col);
    }
  }
  return m3;
}

function transpose(m1) {
  var m2 = makeUninitializedUnsafe(/* () */0);
  for(var row = 0; row <= 1; ++row){
    for(var col = 0; col <= 1; ++col){
      setUnsafe(m2, getUnsafe(m1, row, col), col, row);
    }
  }
  return m2;
}

function determinant(matrix) {
  return getUnsafe(matrix, 0, 0) * getUnsafe(matrix, 1, 1) - getUnsafe(matrix, 0, 1) * getUnsafe(matrix, 1, 0);
}

var m = /* array */[
  /* array */[
    1,
    2,
    3
  ],
  /* array */[
    5.5,
    6.5,
    7.5
  ]
];

if (!Float.equals(getUnsafe(m, 0, 0), 1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "Matrix2D.ml",
          74,
          2
        ]
      ];
}

if (!Float.equals(getUnsafe(m, 0, 2), 3)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "Matrix2D.ml",
          75,
          2
        ]
      ];
}

if (!Float.equals(getUnsafe(m, 1, 0), 5.5)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "Matrix2D.ml",
          76,
          2
        ]
      ];
}

if (!Float.equals(getUnsafe(m, 1, 2), 7.5)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "Matrix2D.ml",
          77,
          2
        ]
      ];
}

var m1 = /* array */[
  /* array */[
    1,
    2,
    3
  ],
  /* array */[
    5,
    6,
    7
  ]
];

var m2 = /* array */[
  /* array */[
    1,
    2,
    3
  ],
  /* array */[
    5,
    6,
    7
  ]
];

if (!equals(m1, m2)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "Matrix2D.ml",
          85,
          2
        ]
      ];
}

var m1$1 = /* array */[
  /* array */[
    1,
    2,
    3
  ],
  /* array */[
    5,
    6,
    7
  ]
];

var m2$1 = /* array */[
  /* array */[
    1,
    6,
    3
  ],
  /* array */[
    4,
    6,
    7
  ]
];

if (equals(m1$1, m2$1)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "Matrix2D.ml",
          93,
          2
        ]
      ];
}

var m1$2 = /* array */[
  /* array */[
    1,
    2
  ],
  /* array */[
    3,
    4
  ]
];

var m2$2 = /* array */[
  /* array */[
    4,
    5
  ],
  /* array */[
    6,
    7
  ]
];

var m3 = /* array */[
  /* array */[
    16,
    19
  ],
  /* array */[
    36,
    43
  ]
];

if (!equals(multiply(m1$2, m2$2), m3)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "Matrix2D.ml",
          103,
          2
        ]
      ];
}

var matrix = /* array */[
  /* array */[
    1,
    2
  ],
  /* array */[
    3,
    4
  ]
];

if (!equals(multiply(matrix, identityMatrix), matrix)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "Matrix2D.ml",
          110,
          2
        ]
      ];
}

var matrix$1 = /* array */[
  /* array */[
    0,
    9
  ],
  /* array */[
    3,
    1
  ]
];

var transposed = /* array */[
  /* array */[
    0,
    3
  ],
  /* array */[
    9,
    1
  ]
];

if (!equals(transpose(matrix$1), transposed)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "Matrix2D.ml",
          119,
          2
        ]
      ];
}

if (!equals(transpose(identityMatrix), identityMatrix)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "Matrix2D.ml",
          122,
          2
        ]
      ];
}

var matrix$2 = /* array */[
  /* array */[
    1,
    5
  ],
  /* array */[
    -3,
    2
  ]
];

if (!Float.equals(determinant(matrix$2), 17)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "Matrix2D.ml",
          129,
          2
        ]
      ];
}

exports.makeUninitializedUnsafe = makeUninitializedUnsafe;
exports.getUnsafe = getUnsafe;
exports.setUnsafe = setUnsafe;
exports.identityMatrix = identityMatrix;
exports.equals = equals;
exports.multiply = multiply;
exports.transpose = transpose;
exports.determinant = determinant;
/*  Not a pure module */