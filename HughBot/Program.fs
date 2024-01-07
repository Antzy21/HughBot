open System.Threading.Tasks
open HughBot
open HughBot.Benchmarking
open System
open System.CommandLine
open System.CommandLine.Invocation

[<EntryPoint>]
let main argv =
    
    Console.OutputEncoding <- System.Text.Encoding.Unicode
    printfn "I'm HughBot, a chess engine."
    
    let benchmarkCommand = Command "benchmark"
    benchmarkCommand.AddAlias("bench")
    let benchmarkArg =
        Argument<string>("type", "Benchmark type to run")
            .FromAmong([|"BRANCHES"; "HEURISTICS"|])
    benchmarkCommand.AddArgument(benchmarkArg)
    benchmarkCommand.SetHandler(RunBenchmarks.RunBenchmark, benchmarkArg)

    let evaluateCommand = Command "evaluate"
    evaluateCommand.AddAlias("eval")
    let fenArgument = Argument<string>("fenArg", "The chess game state, represented as a Fen")
    evaluateCommand.AddArgument(fenArgument)
    evaluateCommand.SetHandler(PlayGame.evaluatePosition, fenArgument)

    let rootCommand = RootCommand "HughBot"
    let depthOption = Option<int> "--depth"
    depthOption.AddAlias("-d")
    depthOption.SetDefaultValue(3)
    rootCommand.AddOption(depthOption)
    let fenOption = Option<string> "--fen"
    fenOption.AddAlias("-f")
    rootCommand.AddOption(fenOption)
    rootCommand.SetHandler(PlayGame.play, fenOption, depthOption)
    
    rootCommand.AddCommand benchmarkCommand
    rootCommand.AddCommand evaluateCommand

    rootCommand.Invoke argv
