namespace HughBot.Benchmarking

open BenchmarkDotNet.Attributes
open Chess
open HughBot
open FSharp.Extensions
open BenchmarkDotNet.Running

[<MemoryDiagnoser>]
type GameBranchesBenchmarking() =

    let game = Game.Create.newGame()

    [<Benchmark>]
    member _.CalculateMovesDepth1() =
        GameTree.calcAndMoveRec 0 true game

    [<Benchmark>]
    member _.CalculateGamesDepth1() = 
        GameTree.calcAndMoveRec 1 false game
        
    [<Benchmark>]
    member _.CalculateMovesDepth2() =
        GameTree.calcAndMoveRec 1 true game

    [<Benchmark>]
    member _.CalculateGamesDepth2() = 
        GameTree.calcAndMoveRec 2 false game
    
[<MemoryDiagnoser>]
type HeursticsBenchmarking() =

    let game = Game.Create.newGame()

    [<Benchmark>]
    member _.Eval() =
        Heuristics.advancedEval game

module RunBenchmarks =

    let chooseBenchmarkToRun () =
        Console.ParseLineWithBreakOption "Which Benchmark do you want to run?\n BRANCHES\n HEURISTICS" (fun (userInput: string) ->
            match userInput.ToUpper() with
            | "BRANCHES" | "B" ->
                BenchmarkRunner.Run<GameBranchesBenchmarking>()
                |> Some 
            | "HEURISTICS" | "H" ->
                BenchmarkRunner.Run<HeursticsBenchmarking>()
                |> Some 
            | _ -> None
            |> Option.map (ignore)
        )