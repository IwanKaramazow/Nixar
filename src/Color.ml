// Should/can we reuse Tuple.t?
type t = {
  red: float,
  green: float,
  blue: float,
}

let make = (~r, ~b, ~g) => {
  red: r,
  blue: b,
  green: g,
}

let toPpmString = (t, max) => {
  let parseNumber = n => {
    let x = max *. n
    if x > max {
      max->Js.Float.toString
    } else if x < 0. {
      "0"
    } else {
      x |> Js.Math.round |> Js.Float.toString
    }
  }
  t.red->parseNumber ++
  " " ++
  t.green->parseNumber ++
  " " ++
  t.blue->parseNumber
}

let equals = (c1, c2) => {
  open Float
  c1.red->equals(c2.red) &&
  c1.blue->equals(c2.blue) &&
  c1.green->equals(c2.green)
}

let add = (c1, c2) => {
  red: c1.red +. c2.red,
  blue: c1.blue +. c2.blue,
  green: c1.green +. c2.green,
}

let subtract = (c1, c2) => {
  red: c1.red -. c2.red,
  blue: c1.blue -. c2.blue,
  green: c1.green -. c2.green,
}

let multScalar = (c, scalar) => {
  red: c.red *. scalar,
  blue: c.blue *. scalar,
  green: c.green *. scalar,
}

let hadamardProduct = (c1, c2) => {
  red: c1.red *. c2.red,
  blue: c1.blue *. c2.blue,
  green: c1.green *. c2.green,
}

// make
let () = {
  let c = make(~r=-0.5, ~g=0.4, ~b=1.7)
  
  assert Float.equals(c.red, -0.5)
  assert Float.equals(c.green, 0.4)
  assert Float.equals(c.blue, 1.7)
}

// adding colors
let () = {
  let c1 = make(~r=0.9, ~b=0.6, ~g=0.75)
  let c2 = make(~r=0.7, ~b=0.1, ~g=0.25)
  
  assert equals(c1->add(c2), make(~r=1.6, ~b=0.7, ~g=1.))
}

// subtract colors
let () = {
  let c1 = make(~r=0.9, ~b=0.6, ~g=0.75)
  let c2 = make(~r=0.7, ~b=0.1, ~g=0.25)
  
  let c3 = make(~r=0.2, ~b=0.5, ~g=0.5)
  
  assert equals(c1->subtract(c2), c3)
}

// multiply scalar
let () = {
  let c1 = make(~r=0.2, ~b=0.3, ~g=0.4)
  let c2 = make(~r=0.4, ~b=0.6, ~g=0.8)
  
  assert equals(c1->multScalar(2.), c2)
}

// multiplying colors
let () = {
  let c1 = make(~r=1., ~g=0.2, ~b=0.4)
  let c2 = make(~r=0.9, ~g=1., ~b=0.1)
  
  let c3 = make(~r=0.9, ~g=0.2, ~b=0.04)
  
  assert equals(c1->hadamardProduct(c2), c3)
}
