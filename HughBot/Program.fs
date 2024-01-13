open System.Threading.Tasks
open Chess
open HughBot
open HughBot.Benchmarking
open System
open System.CommandLine
open System.CommandLine.Invocation

[<EntryPoint>]
let main argv =
    
    Console.OutputEncoding <- System.Text.Encoding.Unicode
    printfn "I'm HughBot, a chess engine."
    
    let depthOption = Option<int> "--depth"
    depthOption.AddAlias("-d")
    depthOption.SetDefaultValue(3)
    
    let orderedEvaluationOption = Option<bool> "--orderedEval"
    orderedEvaluationOption.AddAlias("-e")
    orderedEvaluationOption.SetDefaultValue false
    
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
    evaluateCommand.AddOption(depthOption)
    evaluateCommand.SetHandler(PlayGame.evaluatePosition, fenArgument, depthOption, orderedEvaluationOption)

    let rootCommand = RootCommand "HughBot"
    rootCommand.AddOption(depthOption)
    rootCommand.AddOption(orderedEvaluationOption)
    let colourOption =
        Option<string>("--colour")
            .FromAmong("White", "Black")
    colourOption.AddAlias("-c")
    colourOption.SetDefaultValue("White")
    rootCommand.AddOption(colourOption)
    let opponentOption = Option<string> "--opponent"
    opponentOption.AddAlias("-o")
    rootCommand.AddOption(opponentOption)
    let fenOption = Option<string> "--fen"
    fenOption.AddAlias("-f")
    rootCommand.AddOption(fenOption)
    
    rootCommand.SetHandler(PlayGame.play, fenOption, depthOption, colourOption, opponentOption, orderedEvaluationOption)
    rootCommand.AddCommand benchmarkCommand
    rootCommand.AddCommand evaluateCommand

    rootCommand.Invoke argv
