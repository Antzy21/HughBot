open HughBot
open HughBot.Benchmarking
open BenchmarkDotNet.Running
open FSharp.Extensions

//PlayGame.newGame ()

printfn "I'm HughBot, a chess engine."

Console.ParseLine "What would you like to do? 'Play'/'p' a game or run 'Benchmarks'/'b'" (fun (userInput: string) ->
    match userInput.ToUpper() with
    | "PLAY" | "P" -> 
        PlayGame.newGame ()
        |> Some
    | "BENCHMARKS" | "BENCHMARK" | "BENCH" | "B" -> 
        RunBenchmarks.chooseBenchmarkToRun ()
    | _ -> 
        None
)
