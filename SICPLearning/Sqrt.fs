module Sqrt


let square x = x * x
let abs x = if x > 0.0 then x else -x
let average x y = (x + y) / 2.0

// Newton method to calculate squre root.

let sqrt_v1 x = 
  let rec sqrt_it guess =
    if (abs (square guess - x) / x) < 0.001 then
      guess
    else
      sqrt_it ((guess + x / guess) / 2.0)
  sqrt_it 1.0


let sqrt_v2 x = 
  let close_enough target tolerance root = (abs (target - root) / target) < tolerance
  let avg_damping f = fun x -> average x (f x)
  let getNext = avg_damping (fun v -> x / v)
  let rec sqrt_it guess =
    if square guess |> close_enough x 0.001 then
      guess
    else
      sqrt_it (getNext guess)
  sqrt_it 1.0


let close_enough v1 v2 tolerance = (abs (v1 - v2)) < tolerance
let rec find_fix_point f guess = 
  let next = f guess
  if close_enough next guess 0.001 then
    next
  else
    find_fix_point f next

let sqrt_v3 x = 
  find_fix_point (fun v -> average v  (x / v)) 1.0


let newton_method f = 
  let deriv f dx = fun x -> (f (x + dx) - f x) / dx
  let Df = deriv f 0.001
  find_fix_point (fun x -> x - f x / Df x) 1.0

let sqrt_v4 x =
  newton_method (fun y -> y * y - x)
  

let sqrt_v5 x = 
  let rec search f a b = 
    let close_enough target tolerance value = (abs (target - value)) < tolerance
    let avg = (a + b) / 2.0
    if a - b |> abs |> close_enough 0.0 0.001 then
      avg
    else
      match f avg with
      | v when v > 0.0 -> search f a avg
      | v when v < 0.0 -> search f avg b
      | v -> avg
  let half_interval_method f a b =
    let valueA = f a
    let valueB = f b
    match (valueA, valueB) with
    | (x, y) when x < 0.0 && y > 0.0 -> search f a b
    | (x, y) when x > 0.0 && y < 0.0 -> search f b a
    | (x, y) -> invalidArg "a" "f a must be of different sign with f b."
  half_interval_method (fun v -> v * v - x) 0.0 x
