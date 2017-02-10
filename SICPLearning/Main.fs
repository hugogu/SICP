open Sqrt
open Rational
open Stream
open SymbolicDifferentition
open Microsoft.FSharp.Linq.RuntimeHelpers

[<EntryPoint>]
let main argv = 
    let rec enumerate (low:int, high:int) =
        if (low < high) then Stream(low, fun () -> printfn "generating tail of %A" low; enumerate(low + 1, high)) else Stream.Empty

    let numStream = enumerate(0, 3)

    numStream.iterate (fun item -> printfn "%A" item)

    printfn "sqrt_v1 %A" (sqrt_v1 2.0)
    printfn "sqrt_v2 %A" (sqrt_v2 2.0)
    printfn "sqrt_v3 %A" (sqrt_v3 2.0)
    printfn "sqrt_v4 %A" (sqrt_v4 2.0)
    printfn "sqrt_v5 %A" (sqrt_v5 2.0)
    printfn "sqrt_v6 %A" (sqrt_v6 2.0)

    let sqrts = sqrt_stream 2.0
    sqrts.iterateWhen (fun v -> printfn "sqrt_stream: %A" v) (fun v -> abs(v * v - 2.0) > 0.0001)

    let sqrts_v2 = sqrt_stream_v2 2.0
    sqrts_v2.iterateWhen (fun v -> printfn "sqrt_stream_v2: %A" v) (fun v -> abs(v * v - 2.0) > 0.0001)

    let seg = make_segment (make_point 0.0 0.0) (make_point 3.0 6.0)
    printfn "%A" seg
    printfn "%A" (segment_mid seg)

    let exp = <@ fun (x:float) -> x * x @>
    let der = (derivExp exp)
    printfn "deriv: %A" der
    let derf = LeafExpressionConverter.EvaluateQuotation der :?> ( float -> float )
    printfn "deriv at 2.0: %A" (derf 2.0)

    let rect = make_rect 5.0 8.0
    printfn "round: %A" (rect_round rect)
    printfn "area: %A" (rect_area rect)

    let intervalA = make_center_percent 6.8 0.1
    let intervalB = make_center_percent 4.7 0.05

    printfn "part1: %A" (part1 intervalA intervalB)
    printfn "part2: %A" (part2 intervalA intervalB)

    0
