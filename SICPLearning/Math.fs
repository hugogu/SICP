module Math

let inline square x = x * x
let inline abs x = if x > LanguagePrimitives.GenericZero then x else -x
let inline average x y = (x + y) / (LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne)
let rec gcd a b = if b = 0 then a else gcd b (a % b)