module Math

open LanguagePrimitives

let inline square x = x * x
let inline abs x = if x > GenericZero then x else -x
let inline average x y = (x + y) / (GenericOne + GenericOne)
let rec gcd a b = if b = 0 then a else gcd b (a % b)