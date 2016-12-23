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
let inline add_point a b = make_point (point_x a + point_x b) (point_y a + point_y b)
let inline div_point p n = make_point (point_x p / n) (point_y p / n)

let make_segment = make_point
let segment_start = point_x
let segment_end = point_y

let inline segment_mid s = 
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

// Exercise 2.7 - 2.16
let make_interval (a) (b) = (a, b)
let upper_bound i = match i with | (a, b) -> max a b
let lower_bound i = match i with | (a, b) -> min a b

let inline add_interval x y = 
  make_interval (lower_bound x + lower_bound y) (upper_bound x + upper_bound y)

let inline sub_interval x y =
  make_interval (lower_bound x - lower_bound y) (upper_bound x - upper_bound y)

let inline mul_interval x y =
  let values = [lower_bound x * lower_bound y;
                lower_bound x * upper_bound y;
                upper_bound x * lower_bound y;
                upper_bound x * upper_bound y]
  make_interval (List.min values) (List.max values)

let inline div_interval x y = 
  if ((upper_bound y) * (lower_bound y)) < LanguagePrimitives.GenericZero then
    invalidArg "y" "interval y accross 0."
  else
    make_interval (LanguagePrimitives.GenericOne / upper_bound y) (LanguagePrimitives.GenericOne / lower_bound y) |> mul_interval x 

let inline part1 i1 i2 = 
  div_interval (mul_interval i1 i2) (add_interval i1 i2)

let inline part2 i1 i2 = 
  let one = make_interval LanguagePrimitives.GenericOne LanguagePrimitives.GenericOne
  div_interval one (add_interval (div_interval one i1) (div_interval one i2))

let inline make_center_percent c p =
  make_interval (c * (LanguagePrimitives.GenericOne + p)) (c * (LanguagePrimitives.GenericOne - p))

let inline interval_center i =
  average (upper_bound i) (lower_bound i)

let inline interval_percent i =
  abs (upper_bound i - lower_bound i) / (upper_bound i + lower_bound i)