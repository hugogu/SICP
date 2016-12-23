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

    let intervalA = make_center_percent 6.8 0.1
    let intervalB = make_center_percent 4.7 0.05

    printfn "part1: %A" (part1 intervalA intervalB)
    printfn "part2: %A" (part2 intervalA intervalB)

    0
