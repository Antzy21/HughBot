namespace HughBot.Benchmarking

open BenchmarkDotNet.Attributes
open Chess
open HughBot
open FSharp.Extensions
open BenchmarkDotNet.Running

[<MemoryDiagnoser>]
type GameBranchesBenchmarking() =

    let game = Game.Create.newGame()
        
    [<Benchmark(Baseline=true)>]
    member _.CalculateMovesDepth1() =
        GameTree.branchChessGameState 0 true game

    [<Benchmark>]
    member _.CalculateMovesDepth1Async() =
        GameTree.asyncBranchChessGameState 0 true game

    [<Benchmark>]
    member _.CalculateGamesDepth1() = 
        GameTree.branchChessGameState 1 false game

    [<Benchmark>]
    member _.CalculateGamesDepth1Async() =
        GameTree.asyncBranchChessGameState 1 false game
    
[<MemoryDiagnoser>]
type HeuristicsBenchmarking() =

    let game = Game.Create.newGame()

    [<Benchmark(Baseline=true)>]
    member _.Eval() =
        Heuristics.advancedEval game
        
    [<Benchmark>]
    member _.AsyncEval4() =
        Heuristics.asyncAdvancedEval 4 game
        
    //[<Benchmark>]
    //member _.AsyncEval16() =
    //    Heuristics.asyncAdvancedEval 16 game
        
    //[<Benchmark>]
    //member _.AsyncEval64() =
    //    Heuristics.asyncAdvancedEval 64 game

module RunBenchmarks =

    let RunBenchmark (benchmarkType: string) =
        match benchmarkType with
        | "BRANCHES" | "B" ->
            BenchmarkRunner.Run<GameBranchesBenchmarking>()
            |> ignore
        | "HEURISTICS" | "H" ->
            BenchmarkRunner.Run<HeuristicsBenchmarking>()
            |> ignore
        | _ -> ()
