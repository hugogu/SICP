module SymbolicDifferentition

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

let inline sum e1 e2 = <@ %e1 + %e2 @>
let inline product e1 e2 = <@ %%e1 * %%e2 @>

let rec deriv (exp:Expr) (var:Var) =
    match exp with 
    | Lambda(var, body) -> deriv body var
    | Var(x) -> if x = var then <@ 1.0 @> else <@ 0.0 @>
    | SpecificCall <@ (+) @> (_, _, [lArg;rArg]) -> sum (deriv lArg var) (deriv rArg var)
    | SpecificCall <@ (*) @> (_, _, [lArg;rArg]) -> 
        let pl =  product lArg (deriv rArg var)
        let pr = product rArg (deriv lArg var)
        sum pl pr
    | _ -> invalidArg "exp" ("Invalid exp: " + exp.ToString())

let derivExp (exp:Expr) = 
  let x = Var("x", typeof<float>, false)
  let derivBody = deriv exp x
  <@ fun x -> %derivBody @>
