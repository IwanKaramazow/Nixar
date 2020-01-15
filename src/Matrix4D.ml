type t = array<array<float>> // TODO: could this be made a tuple?

let makeUninitializedUnsafe = () => {
  let matrix = Belt.Array.makeUninitializedUnsafe(4)
  matrix->Belt.Array.setUnsafe(0, Belt.Array.makeUninitializedUnsafe(4))
  matrix->Belt.Array.setUnsafe(1, Belt.Array.makeUninitializedUnsafe(4))
  matrix->Belt.Array.setUnsafe(2, Belt.Array.makeUninitializedUnsafe(4))
  matrix->Belt.Array.setUnsafe(3, Belt.Array.makeUninitializedUnsafe(4))
  
  matrix
}

let getUnsafe = (t: t, ~row, ~col) =>
  t->Belt.Array.getUnsafe(row)->Belt.Array.getUnsafe(col)

let setUnsafe = (t, x, ~row, ~col) =>
  t->Belt.Array.getUnsafe(row)->Belt.Array.setUnsafe(col, x)

let identityMatrix = () => [
  [1., 0., 0., 0.],
  [0., 1., 0., 0.],
  [0., 0., 1., 0.],
  [0., 0., 0., 1.],
]

/* BuckleScript outperforms a classic for-loop: 
   function equals3(m1, m2) {
    for (var i = 0; i < 4; i++) {
        for (var j = 0; j < 4; j++) {
            var x = m1[i][j];
            var y = m2[i][j];
            if(!floatEquals(x, y)) {
                return false
            }
        }
    }
    return true
  } */
let equals = (matrix1, matrix2) => {
  let rec loop = (i, j) =>
    if i > 3 {
      true
    } else {
      let x = matrix1->getUnsafe(~row=i, ~col=j)
      let y = matrix2->getUnsafe(~row=i, ~col=j)
      if !Float.equals(x, y) {
        false
      } else if j < 3 {
        loop(i, j + 1)
      } else {
        loop(i + 1, 0)
      }
    }
  
  loop(0, 0)
}

let multiply = (m1, m2) => {
  let m3 = makeUninitializedUnsafe()
  
  for row in 0 to 3 {
    for col in 0 to 3 {
      m3->setUnsafe(
        m1->getUnsafe(~row, ~col=0) *. m2->getUnsafe(~row=0, ~col) +.
        m1->getUnsafe(~row, ~col=1) *. m2->getUnsafe(~row=1, ~col) +.
        m1->getUnsafe(~row, ~col=2) *. m2->getUnsafe(~row=2, ~col) +.
        m1->getUnsafe(~row, ~col=3) *. m2->getUnsafe(~row=3, ~col),
        ~row,
        ~col,
      )
    }
  }
  
  m3
}

let multiplyTuple = (m1, tuple) =>
  Tuple.makeTuple(
    m1->getUnsafe(~row=0, ~col=0) *. tuple.Tuple.x +.
    m1->getUnsafe(~row=0, ~col=1) *. tuple.Tuple.y +.
    m1->getUnsafe(~row=0, ~col=2) *. tuple.Tuple.z +.
    m1->getUnsafe(~row=0, ~col=3) *. tuple.Tuple.w,
    m1->getUnsafe(~row=1, ~col=0) *. tuple.Tuple.x +.
    m1->getUnsafe(~row=1, ~col=1) *. tuple.Tuple.y +.
    m1->getUnsafe(~row=1, ~col=2) *. tuple.Tuple.z +.
    m1->getUnsafe(~row=1, ~col=3) *. tuple.Tuple.w,
    m1->getUnsafe(~row=2, ~col=0) *. tuple.Tuple.x +.
    m1->getUnsafe(~row=2, ~col=1) *. tuple.Tuple.y +.
    m1->getUnsafe(~row=2, ~col=2) *. tuple.Tuple.z +.
    m1->getUnsafe(~row=2, ~col=3) *. tuple.Tuple.w,
    m1->getUnsafe(~row=3, ~col=0) *. tuple.Tuple.x +.
    m1->getUnsafe(~row=3, ~col=1) *. tuple.Tuple.y +.
    m1->getUnsafe(~row=3, ~col=2) *. tuple.Tuple.z +.
    m1->getUnsafe(~row=3, ~col=3) *. tuple.Tuple.w,
  )

let transpose = matrix => {
  let m2 = Belt.Array.makeUninitializedUnsafe(4)
  m2->Belt.Array.setUnsafe(0, Belt.Array.makeUninitializedUnsafe(4))
  m2->Belt.Array.setUnsafe(1, Belt.Array.makeUninitializedUnsafe(4))
  m2->Belt.Array.setUnsafe(2, Belt.Array.makeUninitializedUnsafe(4))
  m2->Belt.Array.setUnsafe(3, Belt.Array.makeUninitializedUnsafe(4))
  
  for row in 0 to 3 {
    for col in 0 to 3 {
      m2->setUnsafe(matrix->getUnsafe(~row, ~col), ~row=col, ~col=row)
    }
  }
  
  m2
}

let submatrix = (m, ~row, ~col): Matrix3D.t => {
  let subMatrix = Matrix3D.makeUninitializedUnsafe()
  
  for i in 0 to 3 {
    for j in 0 to 3 {
      if !(i == row || j == col) {
        let element = m->getUnsafe(~row=i, ~col=j)
        let rowIx = i >= row ? i - 1 : i
        let colIx = j >= col ? j - 1 : j
        subMatrix->Matrix3D.setUnsafe(element, ~row=rowIx, ~col=colIx)
      }
    }
  }
  
  subMatrix
}

let minor = (matrix, ~row, ~col) =>
  matrix->submatrix(~row, ~col)->Matrix3D.determinant

let cofactor = (matrix, ~row, ~col) => {
  let m = minor(matrix, ~row, ~col)
  mod(row + col, 2) == 0 ? m : -1. *. m
}

let determinant = matrix => {
  let a = matrix->getUnsafe(~row=0, ~col=0)
  let b = matrix->getUnsafe(~row=0, ~col=1)
  let c = matrix->getUnsafe(~row=0, ~col=2)
  let d = matrix->getUnsafe(~row=0, ~col=3)
  
  a *. matrix->cofactor(~row=0, ~col=0) +.
  b *. matrix->cofactor(~row=0, ~col=1) +.
  c *. matrix->cofactor(~row=0, ~col=2) +.
  d *. matrix->cofactor(~row=0, ~col=3)
}

let isInvertible = matrix => determinant(matrix) != 0.

// inversion is the operation that allow you to reverse the effect of multiplication
// it is crucial to the transformation of shapes in a ray tracer
// allowing you to move shapes around, make them bigger or smaller,
// rotate them, etc.
let inverseUnsafe = matrix => {
  assert isInvertible(matrix)
  
  let d = determinant(matrix)
  
  let dest = makeUninitializedUnsafe()
  
  for row in 0 to 3 {
    for col in 0 to 3 {
      let c = cofactor(matrix, ~row, ~col)
      
      dest->setUnsafe(c /. d, ~row=col, ~col=row)
    }
  }
  
  dest
}

// constructing and inspecting a 4x4 matrix
let () = {
  let m = [
    [1., 2., 3., 4.],
    [5.5, 6.5, 7.5, 8.5],
    [9., 10., 11., 12.],
    [13.5, 14.5, 15.5, 16.5],
  ]
  
  assert Float.equals(m->getUnsafe(~row=0, ~col=0), 1.)
  assert Float.equals(m->getUnsafe(~row=0, ~col=3), 4.)
  assert Float.equals(m->getUnsafe(~row=1, ~col=0), 5.5)
  assert Float.equals(m->getUnsafe(~row=1, ~col=2), 7.5)
  assert Float.equals(m->getUnsafe(~row=2, ~col=2), 11.)
  assert Float.equals(m->getUnsafe(~row=3, ~col=0), 13.5)
  assert Float.equals(m->getUnsafe(~row=3, ~col=2), 15.5)
}

// equality check
let () = {
  let m1 = [
    [1., 2., 3., 4.],
    [5., 6., 7., 8.],
    [9., 8., 7., 6.],
    [5., 4., 3., 2.],
  ]
  let m2 = [
    [1., 2., 3., 4.],
    [5., 6., 7., 8.],
    [9., 8., 7., 6.],
    [5., 4., 3., 2.],
  ]
  
  assert (m1->equals(m2))
  
  let m3 = [
    [1., 2., 3., 4.],
    [5., 6., 7., 8.],
    [9., 8., 7., 6.],
    [5., 4., 3., 2.],
  ]
  
  let m4 = [
    [2., 3., 4., 5.],
    [6., 7., 8., 9.],
    [8., 7., 6., 7.],
    [4., 3., 2., 1.],
  ]
  
  assert !(m3->equals(m4))
}

// multiplying two matrices
let () = {
  let m1 = [
    [1., 2., 3., 4.],
    [5., 6., 7., 8.],
    [9., 8., 7., 6.],
    [5., 4., 3., 2.],
  ]
  let m2 = [
    [-2., 1., 2., 3.],
    [3., 2., 1., -1.],
    [4., 3., 6., 5.],
    [1., 2., 7., 8.],
  ]
  
  let m3 = [
    [20., 22., 50., 48.],
    [44., 54., 114., 108.],
    [40., 58., 110., 102.],
    [16., 26., 46., 42.],
  ]
  
  assert equals(m1->multiply(m2), m3)
}

// matrix multiplied by a tuple
let () = {
  let matrix = [
    [1., 2., 3., 4.],
    [2., 4., 4., 2.],
    [8., 6., 4., 1.],
    [0., 0., 0., 1.],
  ]
  
  let b = Tuple.makeTuple(1., 2., 3., 1.)
  
  let c = Tuple.makeTuple(18., 24., 33., 1.)
  
  assert Tuple.equals(matrix->multiplyTuple(b), c)
}

// multiplying a matrix by the identity matrix
let () = {
  let matrix = [
    [0., 1., 2., 4.],
    [1., 2., 4., 8.],
    [2., 4., 8., 16.],
    [4., 8., 16., 32.],
  ]
  
  assert equals(matrix->multiply(identityMatrix()), matrix)
}

// multiplying the identity matrix by a tuple
let () = {
  let a = Tuple.makeTuple(1., 2., 3., 4.)
  
  assert Tuple.equals(identityMatrix()->multiplyTuple(a), a)
}

// transposing matrices
let () = {
  let m = [
    [0., 9., 3., 0.],
    [9., 8., 0., 8.],
    [1., 8., 5., 3.],
    [0., 0., 5., 8.],
  ]
  
  let transposed = [
    [0., 9., 1., 0.],
    [9., 8., 8., 0.],
    [3., 0., 5., 5.],
    [0., 8., 3., 8.],
  ]
  
  assert equals(transpose(m), transposed)
  
  // transposing identity matrix
  assert equals(transpose(identityMatrix()), identityMatrix())
}

// submatrix of 4x4 matrix is a 3x3 matrix
let () = {
  let matrix: t = [
    [-6., 1., 1., 6.],
    [-8., 5., 8., 6.],
    [-1., 0., 8., 2.],
    [-7., 1., -1., 1.],
  ]
  
  let subMatrix: Matrix3D.t = [[-6., 1., 6.], [-8., 8., 6.], [-7., -1., 1.]]
  
  assert Matrix3D.equals(matrix->submatrix(~row=2, ~col=1), subMatrix)
}

// calculating the determinant of a 4x4 matrix
let () = {
  let m = [
    [-2., -8., 3., 5.],
    [-3., 1., 7., 3.],
    [1., 2., -9., 6.],
    [-6., 7., 7., -9.],
  ]
  
  assert Float.equals(cofactor(m, ~row=0, ~col=0), 690.)
  assert Float.equals(cofactor(m, ~row=0, ~col=1), 447.)
  assert Float.equals(cofactor(m, ~row=0, ~col=2), 210.)
  assert Float.equals(cofactor(m, ~row=0, ~col=3), 51.)
  assert Float.equals(determinant(m), -4071.)
}

// testing an invertible matrix for invertibility
let () = {
  let matrix = [
    [6., 4., 4., 4., 4.],
    [5., 5., 7., 6.],
    [4., -9., 3., -7.],
    [9., 1., 7., -6.],
  ]
  
  assert Float.equals(determinant(matrix), -2120.)
  assert isInvertible(matrix)
}

// testing an noninvertible matrix for invertibility
let () = {
  let matrix = [
    [-4., 2., -2., -3.],
    [9., 6., 2., 6.],
    [0., -5., 1., -5.],
    [0., 0., 0., 0.],
  ]
  
  assert Float.equals(determinant(matrix), 0.)
  assert !isInvertible(matrix)
}

// calculating the inverse of a matrix
let () = {
  let a = [
    [-5., 2., 6., -8.],
    [1., -5., 1., 8.],
    [7., 7., -6., -7.],
    [1., -3., 7., 4.],
  ]
  
  let b = inverseUnsafe(a)
  
  assert Float.equals(determinant(a), 532.)
  assert Float.equals(cofactor(a, ~row=2, ~col=3), -160.)
  assert Float.equals(b->getUnsafe(~row=3, ~col=2), -160. /. 532.)
  assert Float.equals(cofactor(a, ~row=3, ~col=2), 105.)
  assert Float.equals(b->getUnsafe(~row=2, ~col=3), 105. /. 532.)
  
  assert equals(
    b,
    [
      [0.21805, 0.45113, 0.24060, -0.04511],
      [-0.80827, -1.45677, -0.44361, 0.52068],
      [-0.07895, -0.22368, -0.05263, 0.19737],
      [-0.52256, -0.81391, -0.30075, 0.30639],
    ],
  )
}

// calculating the inverse of another matrix
let () = {
  let matrix = [
    [8., -5., 9., 2.],
    [7., 5., 6., 1.],
    [-6., 0., 9., 6.],
    [-3., 0., -9., -4.],
  ]
  
  assert equals(
    inverseUnsafe(matrix),
    [
      [-0.15385, -0.15385, -0.28205, -0.53846],
      [-0.07692, 0.12308, 0.02564, 0.03077],
      [0.35897, 0.35897, 0.43590, 0.92308],
      [-0.69231, -0.69231, -0.76923, -1.92308],
    ],
  )
}

// calculating the inverse of a third matrix
let () = {
  let matrix = [
    [9., 3., 0., 9.],
    [-5., -2., -6., -3.],
    [-4., 9., 6., 4.],
    [-7., 6., 6., 2.],
  ]
  
  assert equals(
    inverseUnsafe(matrix),
    [
      [-0.04074, -0.07778, 0.14444, -0.22222],
      [-0.07778, 0.03333, 0.36667, -0.33333],
      [-0.02901, -0.14630, -0.10926, 0.12963],
      [0.17778, 0.06667, -0.26667, 0.33333],
    ],
  )
}

// multiplying a product by its inverse
let () = {
  let a = [
    [3., -9., 7., 3.],
    [3., -8., 2., -9.],
    [-4., 4., 4., 1.],
    [-6., 5., -1., 1.],
  ]
  let b = [
    [8., 2., 2., 2.],
    [3., -1., 7., 0.],
    [7., 0., 5., 4.],
    [6., -2., 0., 5.],
  ]
  
  let c = a->multiply(b)
  
  assert equals(c->multiply(inverseUnsafe(b)), a)
}
