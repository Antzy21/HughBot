open HughBot
open HughBot.Benchmarking
open BenchmarkDotNet.Running

//PlayGame.newGame ()

// Run benchmarks
BenchmarkRunner.Run<HeursticsBenchmarking> ()
|> ignore