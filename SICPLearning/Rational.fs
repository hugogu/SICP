module Rational

open Math

// Practise 2.1
let make_rat n d =
  if d < 0 then
    (-n, -d)
  elif d > 0 then 
    (n, d)
  else invalidArg "d" "denominator can't be 0."
let numer rat = match rat with | (n, d) -> n
let denom rat = match rat with | (n, d) -> d

let add_rat x y = 
  let d = numer x * denom y + numer y * denom x
  let n = denom x * denom y
  make_rat d n

let sub_rat x y = 
  let d = numer x * denom y - numer y * denom x
  let n = denom x * denom y
  make_rat d n

let mul_rat x y =
  make_rat (numer x * numer y) (denom x * denom y)

let div_rat x y =
  make_rat (numer x * denom y) (denom x * numer y)

let equal_rat x y =
  numer x * denom y = denom x * numer y

// Practise 2.2
let make_point x y = (x, y)
let point_x = numer
let point_y = denom
// TODO: keep following two method generic too.
//let add_point a b = make_point (point_x a + point_x b) (point_y a + point_y b)
//let div_point p n = make_point (point_x p / n) (point_y p / n)

let make_segment = make_point
let segment_start = point_x
let segment_end = point_y

let segment_mid s = 
  let startP = segment_start s
  let endP = segment_end s
  make_point (average (point_x startP) (point_x endP)) (average (point_y startP) (point_y endP))

// Practise 2.3 - Option 1
//let make_rect = make_point
//let rect_width = point_x 
//let rect_length = point_y

// Practise 2.3 - Option 2
// This works but pretty unreadable.
let make_rect x y = fun f -> f x y
let rect_width p = p (fun x y -> x)
let rect_length p = p (fun x y -> y)

let rect_round r = 2.0 * (rect_width r + rect_length r)
let rect_area r = 1.0 * rect_width r * rect_length r