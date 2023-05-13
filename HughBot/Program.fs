open HughBot
open HughBot.Benchmarking
open BenchmarkDotNet.Running
open FSharp.Extensions

//PlayGame.newGame ()

printfn "I'm HughBot, a chess engine."

Console.ParseLine "What would you like to do?\n'Play'/'p' a game.\n Run 'Benchmarks'/'b'.\n'Evaluate'/'e' a position." (fun (userInput: string) ->
    match userInput.ToUpper() with
    | "PLAY" | "P" -> 
        PlayGame.newGame ()
        |> Some
    | "BENCHMARKS" | "BENCHMARK" | "BENCH" | "B" -> 
        RunBenchmarks.chooseBenchmarkToRun ()
    | "EVALUATE" | "EVAL" | "E" ->
        PlayGame.evaluatePosition ()
        |> Some
    | _ -> 
        None
)
