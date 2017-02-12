module Timing

open System.Diagnostics

let timeit (f:float -> float) v times = 
    let watch = Stopwatch.StartNew()
    let rec run t : float =
      f v
      if (t > 0) then
         run (t - 1)
      else
         f v
    let res = run times 
    watch.Stop()
    printf "Needed %f ms on " (watch.Elapsed.TotalMilliseconds)
    res