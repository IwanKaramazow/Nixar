type t = Matrix4D.t

let identity = () => Matrix4D.identityMatrix()

let translation = (t, ~x, ~y, ~z): t => {
  let matrix = Matrix4D.identityMatrix()
  matrix->Matrix4D.setUnsafe(x, ~row=0, ~col=3)
  matrix->Matrix4D.setUnsafe(y, ~row=1, ~col=3)
  matrix->Matrix4D.setUnsafe(z, ~row=2, ~col=3)
  
  Matrix4D.multiply(matrix, t)
}

let scaling = (t, ~x, ~y, ~z): t => {
  let matrix = Matrix4D.identityMatrix()
  matrix->Matrix4D.setUnsafe(x, ~row=0, ~col=0)
  matrix->Matrix4D.setUnsafe(y, ~row=1, ~col=1)
  matrix->Matrix4D.setUnsafe(z, ~row=2, ~col=2)
  
  Matrix4D.multiply(matrix, t)
}

// reflection is scaling by a negative value

let rotationX = (t, radians): t => {
  let matrix = Matrix4D.identityMatrix()
  matrix->Matrix4D.setUnsafe(Js.Math.cos(radians), ~row=1, ~col=1)
  matrix->Matrix4D.setUnsafe(Js.Math.sin(radians), ~row=2, ~col=1)
  matrix->Matrix4D.setUnsafe(-.Js.Math.sin(radians), ~row=1, ~col=2)
  matrix->Matrix4D.setUnsafe(Js.Math.cos(radians), ~row=2, ~col=2)
  
  Matrix4D.multiply(matrix, t)
}

let rotationY = (t, radians): t => {
  let matrix = Matrix4D.identityMatrix()
  matrix->Matrix4D.setUnsafe(Js.Math.cos(radians), ~row=0, ~col=0)
  matrix->Matrix4D.setUnsafe(Js.Math.sin(radians), ~row=0, ~col=2)
  matrix->Matrix4D.setUnsafe(-.Js.Math.sin(radians), ~row=2, ~col=0)
  matrix->Matrix4D.setUnsafe(Js.Math.cos(radians), ~row=2, ~col=2)
  
  Matrix4D.multiply(matrix, t)
}

let rotationZ = (t, radians): t => {
  let matrix = Matrix4D.identityMatrix()
  matrix->Matrix4D.setUnsafe(Js.Math.cos(radians), ~row=0, ~col=0)
  matrix->Matrix4D.setUnsafe(Js.Math.sin(radians), ~row=1, ~col=0)
  matrix->Matrix4D.setUnsafe(-.Js.Math.sin(radians), ~row=0, ~col=1)
  matrix->Matrix4D.setUnsafe(Js.Math.cos(radians), ~row=1, ~col=1)
  
  Matrix4D.multiply(matrix, t)
}

let shearing = (t, xy, xz, yx, yz, zx, zy): t => {
  let matrix = Matrix4D.identityMatrix()
  matrix->Matrix4D.setUnsafe(xy, ~row=0, ~col=1)
  matrix->Matrix4D.setUnsafe(xz, ~row=0, ~col=2)
  
  matrix->Matrix4D.setUnsafe(yx, ~row=1, ~col=0)
  matrix->Matrix4D.setUnsafe(yz, ~row=1, ~col=2)
  
  matrix->Matrix4D.setUnsafe(zx, ~row=2, ~col=0)
  matrix->Matrix4D.setUnsafe(zy, ~row=2, ~col=1)
  
  Matrix4D.multiply(matrix, t)
}

// move point
let applyTo = (t, point) => Matrix4D.multiplyTuple(t, point)

// multiplying by a translation matrix
let () = {
  let transform = identity()->translation(~x=5., ~y=-3., ~z=2.)
  
  let point = Tuple.makePoint(-3., 4., 5.)
  
  assert Tuple.equals(transform->applyTo(point), Tuple.makePoint(2., 1., 7.))
}

// multiplying by te inverse of a translation matrix
let () = {
  let transform = identity()->translation(~x=5., ~y=-3., ~z=2.)
  let inv = Matrix4D.inverseUnsafe(transform)
  
  let point = Tuple.makePoint(-3., 4., 5.)
  
  assert Tuple.equals(inv->applyTo(point), Tuple.makePoint(-8., 7., 3.))
}

// translation does not affect vectors
let () = {
  let transform = identity()->translation(~x=5., ~y=-3., ~z=2.)
  let vector = Tuple.makeVector(-3., 4., 5.)
  
  assert Tuple.equals(transform->applyTo(vector), vector)
}

// a scaling  matrix applied to a point
let () = {
  let transform = identity()->scaling(~x=2., ~y=3., ~z=4.)
  let point = Tuple.makePoint(-4., 6., 8.)
  
  assert Tuple.equals(transform->applyTo(point), Tuple.makePoint(-8., 18., 32.))
}

// a scaling matrix applied to a vector
let () = {
  let transform = identity()->scaling(~x=2., ~y=3., ~z=4.)
  let vector = Tuple.makeVector(-4., 6., 8.)
  
  assert Tuple.equals(
    transform->applyTo(vector),
    Tuple.makeVector(-8., 18., 32.),
  )
}

// multiplying by the inverse of a scaling matrix
let () = {
  let transform = identity()->scaling(~x=2., ~y=3., ~z=4.)
  let inv = Matrix4D.inverseUnsafe(transform)
  let vector = Tuple.makeVector(-4., 6., 8.)
  
  assert Tuple.equals(inv->applyTo(vector), Tuple.makeVector(-2., 2., 2.))
}

// reflection is scaling by a negative value
let () = {
  let transform = identity()->scaling(~x=-1., ~y=1., ~z=1.)
  let point = Tuple.makePoint(2., 3., 4.)
  
  assert Tuple.equals(transform->applyTo(point), Tuple.makePoint(-2., 3., 4.))
}

// rotating a point around the x-axis
let () = {
  let point = Tuple.makePoint(0., 1., 0.)
  let halfQuarter = identity()->rotationX(Js.Math._PI /. 4.)
  let fullQuarter = identity()->rotationX(Js.Math._PI /. 2.)
  
  assert Tuple.equals(
    halfQuarter->applyTo(point),
    Tuple.makePoint(0., Js.Math.sqrt(2.) /. 2., Js.Math.sqrt(2.) /. 2.),
  )
  assert Tuple.equals(fullQuarter->applyTo(point), Tuple.makePoint(0., 0., 1.))
}

// the inverse of an x-rotation in the opposite direction
let () = {
  let point = Tuple.makePoint(0., 1., 0.)
  
  let halfQuarter = identity()->rotationX(Js.Math._PI /. 4.)
  let inv = Matrix4D.inverseUnsafe(halfQuarter)
  
  assert Tuple.equals(
    inv->applyTo(point),
    Tuple.makePoint(0., Js.Math.sqrt(2.) /. 2., -.Js.Math.sqrt(2.) /. 2.),
  )
}

// rotating a point around the y axis
let () = {
  let point = Tuple.makePoint(0., 0., 1.)
  let halfQuarter = identity()->rotationY(Js.Math._PI /. 4.)
  let fullQuarter = identity()->rotationY(Js.Math._PI /. 2.)
  
  assert Tuple.equals(
    halfQuarter->applyTo(point),
    Tuple.makePoint(Js.Math.sqrt(2.) /. 2., 0., Js.Math.sqrt(2.) /. 2.),
  )
  
  assert Tuple.equals(fullQuarter->applyTo(point), Tuple.makePoint(1., 0., 0.))
}

// rotating a point around the z axis
let () = {
  let point = Tuple.makePoint(0., 1., 0.)
  let halfQuarter = identity()->rotationZ(Js.Math._PI /. 4.)
  let fullQuarter = identity()->rotationZ(Js.Math._PI /. 2.)
  
  assert Tuple.equals(
    halfQuarter->applyTo(point),
    Tuple.makePoint(-.Js.Math.sqrt(2.) /. 2., Js.Math.sqrt(2.) /. 2., 0.),
  )
  
  assert Tuple.equals(fullQuarter->applyTo(point), Tuple.makePoint(-1., 0., 0.))
}

// a shearing transformation moves x in proportion to y
let () = {
  let transform = identity()->shearing(1., 0., 0., 0., 0., 0.)
  
  let point = Tuple.makePoint(2., 3., 4.)
  
  assert Tuple.equals(transform->applyTo(point), Tuple.makePoint(5., 3., 4.))
}

// a shearing transformation moves x in proportion to z
let () = {
  let transform = identity()->shearing(0., 1., 0., 0., 0., 0.)
  
  let point = Tuple.makePoint(2., 3., 4.)
  
  assert Tuple.equals(transform->applyTo(point), Tuple.makePoint(6., 3., 4.))
}

// a shearing transformation moves y in proportion to x
let () = {
  let transform = identity()->shearing(0., 0., 1., 0., 0., 0.)
  
  let point = Tuple.makePoint(2., 3., 4.)
  
  assert Tuple.equals(transform->applyTo(point), Tuple.makePoint(2., 5., 4.))
}

// a shearing transformation moves y in proportion to z
let () = {
  let transform = identity()->shearing(0., 0., 0., 1., 0., 0.)
  
  let point = Tuple.makePoint(2., 3., 4.)
  
  assert Tuple.equals(transform->applyTo(point), Tuple.makePoint(2., 7., 4.))
}

// a shearing transformation moves z in proportion to x
let () = {
  let transform = identity()->shearing(0., 0., 0., 0., 1., 0.)
  
  let point = Tuple.makePoint(2., 3., 4.)
  
  assert Tuple.equals(transform->applyTo(point), Tuple.makePoint(2., 3., 6.))
}

// a shearing transformation moves z in proportion to x
let () = {
  let transform = identity()->shearing(0., 0., 0., 0., 1., 0.)
  
  let point = Tuple.makePoint(2., 3., 4.)
  
  assert Tuple.equals(transform->applyTo(point), Tuple.makePoint(2., 3., 6.))
}

// a shearing transformation moves z in proportion to y
let () = {
  let transform = identity()->shearing(0., 0., 0., 0., 0., 1.)
  
  let point = Tuple.makePoint(2., 3., 4.)
  
  assert Tuple.equals(transform->applyTo(point), Tuple.makePoint(2., 3., 7.))
}

// inidividual transformations are applied in a seq
let () = {
  let point = Tuple.makePoint(1., 0., 1.)
  let a = identity()->rotationX(Js.Math._PI /. 2.)
  let b = identity()->scaling(~x=5., ~y=5., ~z=5.)
  let c = identity()->translation(~x=10., ~y=5., ~z=7.)
  
  // apply rotation first
  let point2 = a->applyTo(point)
  assert Tuple.equals(point2, Tuple.makePoint(1., -1., 0.))
  
  // then apply scaling
  let point3 = b->applyTo(point2)
  assert Tuple.equals(point3, Tuple.makePoint(5., -5., 0.))
  
  // then apply translation
  let point4 = c->applyTo(point3)
  assert Tuple.equals(point4, Tuple.makePoint(15., 0., 7.))
}

// chained transformations must be applied in reverse order
let () = {
  let point = Tuple.makePoint(1., 0., 1.)
  
  assert Tuple.equals(
    identity()
    ->rotationX(Js.Math._PI /. 2.)
    ->scaling(~x=5., ~y=5., ~z=5.)
    ->translation(~x=10., ~y=5., ~z=7.)
    ->applyTo(point),
    Tuple.makePoint(15., 0., 7.),
  )
}
