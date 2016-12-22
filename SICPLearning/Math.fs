module Math

let square x:float = x * x
let abs x = if x > 0.0 then x else -x
let average x y = (x + y) / 2.0
let rec gcd a b = if b = 0 then a else gcd b (a % b)