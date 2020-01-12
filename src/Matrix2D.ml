type t = array<array<float>> // TODO: could this be made a tuple?

let makeUninitializedUnsafe = () => {
  let matrix = Belt.Array.makeUninitializedUnsafe(2)
  matrix->Belt.Array.setUnsafe(0, Belt.Array.makeUninitializedUnsafe(2))
  matrix->Belt.Array.setUnsafe(1, Belt.Array.makeUninitializedUnsafe(2))
  matrix
}

let getUnsafe = (t, ~row, ~col) =>
  t->Belt.Array.getUnsafe(row)->Belt.Array.getUnsafe(col)

let setUnsafe = (t, x, ~row, ~col) =>
  t->Belt.Array.getUnsafe(row)->Belt.Array.setUnsafe(col, x)

let identityMatrix = [[1., 0.], [0., 1.]]

let equals = (matrix1, matrix2) => {
  let rec loop = (i, j) =>
    if i > 1 {
      true
    } else {
      let x = matrix1->getUnsafe(~row=i, ~col=j)
      let y = matrix2->getUnsafe(~row=i, ~col=j)
      if !Float.equals(x, y) {
        false
      } else if j < 1 {
        loop(i, j + 1)
      } else {
        loop(i + 1, 0)
      }
    }
  
  loop(0, 0)
}

let multiply = (m1, m2) => {
  let m3 = makeUninitializedUnsafe()
  
  for row in 0 to 1 {
    for col in 0 to 1 {
      m3->setUnsafe(
        m1->getUnsafe(~row, ~col=0) *. m2->getUnsafe(~row=0, ~col) +.
          m1->getUnsafe(~row, ~col=1) *. m2->getUnsafe(~row=1, ~col),
        ~row,
        ~col,
      )
    }
  }
  
  m3
}

let transpose = m1 => {
  let m2 = makeUninitializedUnsafe()
  
  for row in 0 to 1 {
    for col in 0 to 1 {
      m2->setUnsafe(m1->getUnsafe(~col, ~row), ~row=col, ~col=row)
    }
  }
  
  m2
}

let determinant = matrix =>
  matrix->getUnsafe(~row=0, ~col=0) *. matrix->getUnsafe(~row=1, ~col=1) -.
    matrix->getUnsafe(~row=0, ~col=1) *. matrix->getUnsafe(~row=1, ~col=0)

// constructing and inspecting a 2x2 matrix
let () = {
  let m = [[1., 2., 3.], [5.5, 6.5, 7.5]]
  
  assert Float.equals(m->getUnsafe(~row=0, ~col=0), 1.)
  assert Float.equals(m->getUnsafe(~row=0, ~col=2), 3.)
  assert Float.equals(m->getUnsafe(~row=1, ~col=0), 5.5)
  assert Float.equals(m->getUnsafe(~row=1, ~col=2), 7.5)
}

// equality check
let () = {
  let m1 = [[1., 2., 3.], [5., 6., 7.]]
  let m2 = [[1., 2., 3.], [5., 6., 7.]]
  
  assert (m1->equals(m2))
}

// not equal check
let () = {
  let m1 = [[1., 2., 3.], [5., 6., 7.]]
  let m2 = [[1., 6., 3.], [4., 6., 7.]]
  
  assert !(m1->equals(m2))
}

// multiplying two matrices
let () = {
  let m1 = [[1., 2.], [3., 4.]]
  let m2 = [[4., 5.], [6., 7.]]
  
  let m3 = [[16., 19.], [36., 43.]]
  
  assert equals(m1->multiply(m2), m3)
}

// multiplying the identity matrix
let () = {
  let matrix = [[1., 2.], [3., 4.]]
  
  assert equals(matrix->multiply(identityMatrix), matrix)
}

// transposing matrices
let () = {
  let matrix = [[0., 9.], [3., 1.]]
  
  let transposed = [[0., 3.], [9., 1.]]
  
  assert equals(transpose(matrix), transposed)
  
  // identity matrix
  assert equals(transpose(identityMatrix), identityMatrix)
}

// determinant of a 2x2 matrix
let () = {
  let matrix = [[1., 5.], [-3., 2.]]
  
  assert Float.equals(determinant(matrix), 17.)
}
