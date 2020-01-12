type t = array<array<float>> // TODO: could this be made a tuple?

let makeUninitializedUnsafe = () => {
  let matrix = Belt.Array.makeUninitializedUnsafe(3)
  matrix->Belt.Array.setUnsafe(0, Belt.Array.makeUninitializedUnsafe(3))
  matrix->Belt.Array.setUnsafe(1, Belt.Array.makeUninitializedUnsafe(3))
  matrix->Belt.Array.setUnsafe(2, Belt.Array.makeUninitializedUnsafe(3))
  matrix
}

let getUnsafe = (t, ~row, ~col) =>
  t->Belt.Array.getUnsafe(row)->Belt.Array.getUnsafe(col)

let setUnsafe = (t, x, ~row, ~col) =>
  t->Belt.Array.getUnsafe(row)->Belt.Array.setUnsafe(col, x)

let identityMatrix = [[1., 0., 0.], [0., 1., 0.], [0., 0., 1.]]

let equals = (matrix1, matrix2) => {
  let rec loop = (i, j) =>
    if i > 2 {
      true
    } else {
      let x = matrix1->getUnsafe(~row=i, ~col=j)
      let y = matrix2->getUnsafe(~row=i, ~col=j)
      if !Float.equals(x, y) {
        false
      } else if j < 2 {
        loop(i, j + 1)
      } else {
        loop(i + 1, 0)
      }
    }
  
  loop(0, 0)
}

let multiply = (m1, m2) => {
  let m3 = makeUninitializedUnsafe()
  
  for row in 0 to 2 {
    for col in 0 to 2 {
      m3->setUnsafe(
        m1->getUnsafe(~row, ~col=0) *. m2->getUnsafe(~row=0, ~col) +.
        m1->getUnsafe(~row, ~col=1) *. m2->getUnsafe(~row=1, ~col) +.
        m1->getUnsafe(~row, ~col=2) *. m2->getUnsafe(~row=2, ~col),
        ~row,
        ~col,
      )
    }
  }
  
  m3
}

let transpose = matrix => {
  let transposed = makeUninitializedUnsafe()
  
  for i in 0 to 2 {
    for j in 0 to 2 {
      transposed->setUnsafe(matrix->getUnsafe(~row=i, ~col=j), ~row=j, ~col=i)
    }
  }
  
  transposed
}

let submatrix = (matrix, ~row, ~col): Matrix2D.t => {
  let subMatrix = Matrix2D.makeUninitializedUnsafe()
  
  for i in 0 to 2 {
    for j in 0 to 2 {
      if !(i == row || j == col) {
        let element = matrix->getUnsafe(~row=i, ~col=j)
        let rowIx = i >= row ? i - 1 : i
        let colIx = j >= col ? j - 1 : j
        
        subMatrix->setUnsafe(element, ~row=rowIx, ~col=colIx)
      }
    }
  }
  
  subMatrix
}

let minor = (matrix, ~row, ~col) =>
  matrix->submatrix(~row, ~col)->Matrix2D.determinant

let cofactor = (matrix, ~row, ~col) => {
  let m = minor(matrix, ~row, ~col)
  
  mod(row + col, 2) == 0 ? m : -1. *. m
}

let determinant = matrix => {
  let a = matrix->getUnsafe(~row=0, ~col=0)
  let b = matrix->getUnsafe(~row=0, ~col=1)
  let c = matrix->getUnsafe(~row=0, ~col=2)
  
  a *. matrix->cofactor(~row=0, ~col=0) +.
  b *. matrix->cofactor(~row=0, ~col=1) +.
  c *. matrix->cofactor(~row=0, ~col=2)
}

// constructing and inspecting a 4x4 matrix
let () = {
  let m = [[1., 2., 3.], [5.5, 6.5, 7.5], [9., 10., 11.]]
  
  assert Float.equals(m->getUnsafe(~row=0, ~col=0), 1.)
  assert Float.equals(m->getUnsafe(~row=0, ~col=2), 3.)
  assert Float.equals(m->getUnsafe(~row=1, ~col=0), 5.5)
  assert Float.equals(m->getUnsafe(~row=1, ~col=2), 7.5)
  assert Float.equals(m->getUnsafe(~row=2, ~col=2), 11.)
}

// equality check
let () = {
  let m1 = [[1., 2., 3.], [5., 6., 7.], [9., 8., 7.]]
  let m2 = [[1., 2., 3.], [5., 6., 7.], [9., 8., 7.]]
  
  assert (m1->equals(m2))
}

// not equal
let () = {
  let m1 = [[1., 2., 3.], [5., 6., 7.], [9., 8., 7.]]
  let m2 = [[9., 2., 3.], [8., 6., 7.], [9., 8., 7.]]
  
  assert !(m1->equals(m2))
}

// multiply matrix
let () = {
  let m1 = [[1., 2., 3.], [3., 2., 1.], [2., 4., 6.]]
  let m2 = [[2., 4., 6.], [1., 2., 3.], [3., 2., 1.]]
  
  let m3 = [[13., 14., 15.], [11., 18., 25.], [26., 28., 30.]]
  
  assert equals(m1->multiply(m2), m3)
  
  // identity matrix
  assert equals(m1->multiply(identityMatrix), m1)
  assert equals(m2->multiply(identityMatrix), m2)
  assert equals(m3->multiply(identityMatrix), m3)
  assert equals(identityMatrix->multiply(identityMatrix), identityMatrix)
}

let () = {
  let matrix = [[1., 2., 3.], [3., 2., 1.], [2., 4., 6.]]
  let transposed = [[1., 3., 2.], [2., 2., 4.], [3., 1., 6.]]
  
  assert equals(transpose(matrix), transposed)
  assert equals(transpose(identityMatrix), identityMatrix)
}

// submatrix of a 3x3 matrix is a 2x2 matrix
let () = {
  let matrix: t = [[1., 5., 0.], [-3., 2., 7.], [0., 6., -3.]]
  
  let subMatrix: Matrix2D.t = [[-3., 2.], [0., 6.]]
  
  assert Matrix2D.equals(matrix->submatrix(~row=0, ~col=2), subMatrix)
}

// calculating a minor of a 3x3 matrix
let () = {
  let a = [[3., 5., 0.], [2., -1., -7.], [6., -1., 5.]]
  
  let b = a->submatrix(~row=1, ~col=0)
  
  assert Float.equals(Matrix2D.determinant(b), minor(a, ~row=1, ~col=0))
}

// calculating a cofactor of a 3x3 matrix
let () = {
  let a = [[3., 5., 0.], [2., -1., -7.], [6., -1., 5.]]
  
  assert Float.equals(minor(a, ~row=0, ~col=0), -12.)
  assert Float.equals(cofactor(a, ~row=0, ~col=0), -12.)
  
  assert Float.equals(minor(a, ~row=1, ~col=0), 25.)
  
  assert Float.equals(cofactor(a, ~row=1, ~col=0), -25.)
}

// calculating the determinant of a 3x3 matrix
let () = {
  let m = [[1., 2., 6.], [-5., 8., -4.], [2., 6., 4.]]
  
  assert Float.equals(cofactor(m, ~row=0, ~col=0), 56.)
  assert Float.equals(cofactor(m, ~row=0, ~col=1), 12.)
  assert Float.equals(cofactor(m, ~row=0, ~col=2), -46.)
  assert Float.equals(determinant(m), -196.)
}
