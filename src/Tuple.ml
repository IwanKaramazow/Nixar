type t = {
  x: float,
  y: float,
  z: float,
  w: float,
}

let zeroVector = {x: 0., y: 0., z: 0., w: 0.}

let equals = (a, b) =>
  Float.equals(a.x, b.x) &&
  Float.equals(a.y, b.y) &&
  Float.equals(a.z, b.z) &&
  Float.equals(a.w, b.w)

let makeTuple = (x, y, z, w) => {
  x: x,
  y: y,
  z: z,
  w: w,
}

let makePoint = (x, y, z) => {
  x: x,
  y: y,
  z: z,
  w: 1.,
}

let makeVector = (x, y, z) => {
  x: x,
  y: y,
  z: z,
  w: 0.,
}

let isPoint = ({w}) => w == 1.
let isVector = ({w}) => w == 0.

let add = (a, b) => {
  x: a.x +. b.x,
  y: a.y +. b.y,
  z: a.z +. b.z,
  w: a.w +. b.w,
}

// conceptually is moving backwards????
let subtract = (a, b) => {
  x: a.x -. b.x,
  y: a.y -. b.y,
  z: a.z -. b.z,
  w: a.w -. b.w,
}

let negate = t => {
  x: -.t.x,
  y: -.t.y,
  z: -.t.z,
  w: -.t.w,
}

// what point lies 3.5 times farther in that direction?
let multiplyScalar = (t, scalar) => {
  x: t.x *. scalar,
  y: t.y *. scalar,
  z: t.z *. scalar,
  w: t.w *. scalar,
}

let divideScalar = (t, scalar) => {
  x: t.x /. scalar,
  y: t.y /. scalar,
  z: t.z /. scalar,
  w: t.w /. scalar,
}

// how far you would travel in a straight line if you were to walk from one end of the vector to the other one
// distance === magnitude === length of the vector
let magnitude = t =>
  Js.Math.sqrt(t.x *. t.x +. t.y *. t.y +. t.z *. t.z +. t.w *. t.w)

// unit vector = vector with magnitude 1
let isUnitVector = t => Float.equals(magnitude(t), 1.)

// normalization = process of taking an arbitrary vector and converting it into a unit vector
// keeps your calculations anchored relative to a common scale (the unit vector)
let normalize = v => {
  let m = v->magnitude
  {x: v.x /. m, y: v.y /. m, z: v.z /. m, w: v.w /. m}
}

// dot product = scalar product = inner product
// takes two vectors and returns a scalar value
// the smaller the dot product, the larger the angle between the vectors
let dot = (a, b) => a.x *. b.x +. a.y *. b.y +. a.z *. b.z +. a.w *. b.w

let cross = (a, b) =>
  makeVector(
    a.y *. b.z -. a.z *. b.y,
    a.z *. b.x -. a.x *. b.z,
    a.x *. b.y -. a.y *. b.x,
  )

let () = {
  let a = makeTuple(4.3, -4.2, 3.1, 1.)
  assert (a.x == 4.3 && a.y == -4.2 && a.z == 3.1 && a.w == 1.)
  assert !(a->isVector)
  assert (a->isPoint)
}

let () = {
  let a = makeTuple(4.3, -4.2, 3.1, 0.)
  assert (a.x == 4.3 && a.y == -4.2 && a.z == 3.1 && a.w == 0.)
  assert !(a->isPoint)
  assert (a->isVector)
}

let () = {
  let p = makePoint(4., -4., 3.)
  assert equals(p, makeTuple(4., -4., 3., 1.))
  
  let v = makeVector(4., -4., 3.)
  assert equals(v, makeTuple(4., -4., 3., 0.))
}

let () = {
  let a = makeTuple(3., -2., 5., 1.)
  let b = makeTuple(-2., 3., 1., 0.)
  let c = makeTuple(1., 1., 6., 1.)
  assert equals(add(a, b), c)
}

// subtracting two points
let () = {
  let p1 = makePoint(3., 2., 1.)
  let p2 = makePoint(5., 6., 7.)
  let vec = makeVector(-2., -4., -6.)
  
  assert equals(subtract(p1, p2), vec)
}

// subtracting a vector from a point
let () = {
  let p = makePoint(3., 2., 1.)
  let v = makeVector(5., 6., 7.)
  let diff = makePoint(-2., -4., -6.)
  
  assert equals(subtract(p, v), diff)
}

// subtracting two vectors
let () = {
  let v1 = makeVector(3., 2., 1.)
  let v2 = makeVector(5., 6., 7.)
  
  let v3 = makeVector(-2., -4., -6.)
  
  assert equals(v1->subtract(v2), v3)
}

// subtracting a vector from the zero vector
let () = {
  let vec = makeVector(1., -2., 3.)
  let expected = makeVector(-1., 2., -3.)
  
  assert equals(zeroVector->subtract(vec), expected)
}

// negating a tuple
let () = {
  let a = makeTuple(1., -2., 3., -4.)
  assert equals(a->negate, makeTuple(-1., 2., -3., 4.))
}

// multiplying a tuple by a scalar
let () = {
  let a = makeTuple(1., -2., 3., -4.)
  
  assert equals(a->multiplyScalar(3.5), makeTuple(3.5, -7., 10.5, -14.))
}

// multiplying a tuple by a fractian
let () = {
  let a = makeTuple(1., -2., 3., -4.)
  
  assert equals(a->multiplyScalar(0.5), makeTuple(0.5, -1., 1.5, -2.))
}

// dividing a tuple by a scalar
let () = {
  let a = makeTuple(1., -2., 3., -4.)
  
  assert equals(a->divideScalar(2.), makeTuple(0.5, -1., 1.5, -2.))
}

// magnitude of vector(1, 0, 0)
let () = {
  let v = makeVector(1., 0., 0.)
  assert Float.equals(v->magnitude, 1.)
}

// magnitude of vector(0, 1, 0)
let () = {
  let v = makeVector(0., 1., 0.)
  assert Float.equals(v->magnitude, 1.)
}

// magnitude of vector(0, 0, 1)
let () = {
  let v = makeVector(0., 0., 1.)
  assert Float.equals(v->magnitude, 1.)
}

// magnitude of vector(1, 2, 3)
let () = {
  let v = makeVector(1., 2., 3.)
  assert Float.equals(v->magnitude, Js.Math.sqrt(14.))
}

// magnitude of vector(-1, -2, -3)
let () = {
  let v = makeVector(-1., -2., -3.)
  assert Float.equals(v->magnitude, Js.Math.sqrt(14.))
}

// normalizing vector(4, 0, 0)
let () = {
  let v = makeVector(4., 0., 0.)
  assert equals(v->normalize, makeVector(1., 0., 0.))
}

// magnitude of a normalized vector
let () = {
  let v = makeVector(1., 2., 3.)
  let norm = v->normalize
  assert Float.equals(norm->magnitude, 1.)
}

// dot product of two tuples
let () = {
  let a = makeVector(1., 2., 3.)
  let b = makeVector(2., 3., 4.)
  assert Float.equals(dot(a, b), 20.)
}

// the cross product of two vectors
let () = {
  let a = makeVector(1., 2., 3.)
  let b = makeVector(2., 3., 4.)
  
  assert equals(a->cross(b), makeVector(-1., 2., -1.))
  assert equals(b->cross(a), makeVector(1., -2., 1.))
}