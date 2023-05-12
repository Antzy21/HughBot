open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Chess

type GameTree =
    | Branches of GameTree list
    | Game of game
    | Moves of move list

module GameTree =

    let calcAndMove (game: game) =
        GameState.getMoves game.gameState
        |> List.map (fun move ->
            Game.Update.makeMove move game 
        )

    let rec calcAndMoveRec (depth: int) (finishWithMoves: bool) (game: game) =
        if depth <= 0 then
            if finishWithMoves then
                Moves (GameState.getMoves game.gameState)
            else
                Game game
        else
            calcAndMove game
            |> List.map (calcAndMoveRec (depth-1) finishWithMoves)
            |> Branches
    
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

BenchmarkRunner.Run<HeursticsBenchmarking> ()
|> ignore