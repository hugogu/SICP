open Sqrt
open Rational

[<EntryPoint>]
let main argv = 
    printfn "%A" (sqrt_v1 2.0)
    printfn "%A" (sqrt_v2 2.0)
    printfn "%A" (sqrt_v3 2.0)
    printfn "%A" (sqrt_v4 2.0)
    printfn "%A" (sqrt_v5 2.0)

    let seg = make_segment (make_point 0.0 0.0) (make_point 3.0 6.0)
    printfn "%A" seg
    printfn "%A" (segment_mid seg)

    let rect = make_rect 5.0 8.0
    printfn "round: %A" (rect_round rect)
    printfn "area: %A" (rect_area rect)

    0
