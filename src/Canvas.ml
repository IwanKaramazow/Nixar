// A canvas is a kind of virtual drawing board, which the ray tracer will use to turn your scenes into images
type t = {
  width: int,
  height: int,
  pixels: array<array<Color.t>>,
}

let make = (~width, ~height) => {
  let pixelArray = Belt.Array.makeUninitializedUnsafe(height)
  for y in 0 to height - 1 {
    let row = []
    
    for x in 0 to width - 1 {
      row->Belt.Array.setUnsafe(x, Color.make(~r=0., ~g=0., ~b=0.))
    }
    
    pixelArray->Belt.Array.setUnsafe(y, row)
  }
  
  {width: width, height: height, pixels: pixelArray}
}

let writePixel = (t, ~x, ~y, ~color) =>
  t.pixels->Belt.Array.getUnsafe(y)->Belt.Array.setUnsafe(x, color)

let pixelAt = (t, ~x, ~y) =>
  t.pixels->Js.Array2.unsafe_get(y)->Js.Array2.unsafe_get(x)

let toPpm = t => {
  let line0 = "P3\n"
  let line1 = t.width->string_of_int ++ " " ++ t.height->string_of_int ++ "\n"
  let line2 = "255\n"
  
  let header = line0 ++ line1 ++ line2
  
  let pixelData = ref("")
  
  for y in 0 to t.height - 1 {
    let row = t.pixels->Belt.Array.getUnsafe(y)
    for x in 0 to t.width - 1 {
      let pixel = row->Belt.Array.getUnsafe(x)
      pixelData := (&pixelData ++ Color.toPpmString(pixel, 255.) ++ " ")
    }
    pixelData := (&pixelData ++ "\n")
  }
  
  header ++ &pixelData
}

let writeToFile = canvas => {
  let data = toPpm(canvas)
  Node.Fs.writeFileAsUtf8Sync("canvas.ppm", data)
}

// creating a canvas
let () = {
  let c = make(~width=10, ~height=20)
  
  assert (c.width == 10)
  assert (c.height == 20)
  assert (c.pixels->Belt.Array.length == 20)
  assert (c.pixels->Belt.Array.getUnsafe(0)->Belt.Array.length == 10)
  
  for x in 0 to 9 {
    for y in 0 to 19 {
      assert Color.equals(c->pixelAt(~x, ~y), Color.make(~r=0., ~g=0., ~b=0.))
    }
  }
}

// writing pixels to a canvas
let () = {
  let canvas = make(~width=10, ~height=20)
  let red = Color.make(~r=1., ~g=0., ~b=0.)
  
  canvas->writePixel(~x=2, ~y=3, ~color=red)
  
  assert Color.equals(canvas->pixelAt(~x=2, ~y=3), red)
}

// constructing ppm header
let () = {
  let canvas = make(~width=5, ~height=3)
  
  let ppm = toPpm(canvas)
  
  let lines = ppm->Js.String2.split("\n")
  
  assert (lines->Belt.Array.getUnsafe(0) == "P3")
  assert (lines->Belt.Array.getUnsafe(1) == "5 3")
  assert (lines->Belt.Array.getUnsafe(2) == "255")
}

// constructing PPM pixel data
let () = {
  let canvas = make(~width=5, ~height=3)
  let c1 = Color.make(~r=1.5, ~g=0., ~b=0.)
  let c2 = Color.make(~r=0., ~g=0.5, ~b=0.)
  let c3 = Color.make(~r=-0.5, ~g=0., ~b=1.)
  
  canvas->writePixel(~x=0, ~y=0, ~color=c1)
  canvas->writePixel(~x=2, ~y=1, ~color=c2)
  canvas->writePixel(~x=4, ~y=2, ~color=c3)
  
  let ppm = toPpm(canvas)
  
  let lines = ppm->Js.String2.split("\n")
  
  lines->Belt.Array.getUnsafe(3)->Js.log
  lines->Belt.Array.getUnsafe(4)->Js.log
  lines->Belt.Array.getUnsafe(5)->Js.log
  
  // assert (lines->Belt.Array.getUnsafe(3) == "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0")
  
  // assert (lines->Belt.Array.getUnsafe(4) == "5 3")
  // assert (lines->Belt.Array.getUnsafe(5) == "255")
  
  writeToFile(canvas)
}
