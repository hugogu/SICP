module SymbolicDifferentition

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

let inline sum     e1 e2 = <@@ (%%e1: float) + (%%e2: float) @@>
let inline sub     e1 e2 = <@@ (%%e1: float) - (%%e2: float) @@>
let inline product e1 e2 = <@@ (%%e1: float) * (%%e2: float) @@>

let rec deriv (body:Expr) (p:Var) =
    match body with 
    | Var(x) ->
        if x = p then <@@ 1.0 @@> else <@@ 0.0 @@>
    | ValueWithName(x) ->
        <@@ 0.0 @@>
    | SpecificCall <@ (+) @> (_, _, [lArg;rArg]) ->
        sum (deriv lArg p) (deriv rArg p)
    | SpecificCall <@ (-) @> (_, _, [lArg;rArg]) ->
        sub (deriv lArg p) (deriv rArg p)
    | SpecificCall <@ (*) @> (_, _, [lArg;rArg]) -> 
        let pl = product lArg (deriv rArg p)
        let pr = product rArg (deriv lArg p)
        sum pl pr
    | _ -> invalidArg "body" ("Invalid body: " + body.ToString())

let derivExp (exp:Expr) = 
    let (var, body) =
        match exp with 
        | Lambda(var, body) -> (var, body)
        | _ -> invalidArg "exp" ("Invalid exp: " + exp.ToString())
    let derivBody = deriv body var
    Expr.Lambda(var, derivBody)
