open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Chess

type GameTree =
    | Branches of GameTree list
    | Node of game

module GameTree =

    let getMoves gametree =  
        match gametree with
        | Node g -> GameState.getMoves g.gameState
        | _ -> failwith ""

    let calcAndMove (game: game) =
        GameState.getMoves game.gameState
        |> List.map (fun move ->
            Game.Update.makeMove move game 
        )

    let rec calcAndMoveRec (depth: int) (game: game) =
        if depth <= 0 then
            Node game
        else
            calcAndMove game
            |> List.map (calcAndMoveRec (depth-1))
            |> Branches
    
[<MemoryDiagnoser>]
type GameBranchesBenchmarking() =

    let game = Game.Create.newGame()

    [<Benchmark>]
    member _.CalculateMovesDepth1() =
        GameState.getMoves game.gameState

    [<Benchmark>]
    member _.CalculateMovesDepth1AndMakeMove() = 
        GameTree.calcAndMoveRec 1 game
        
    [<Benchmark>]
    member _.CalculateMovesDepth2() =
        GameTree.calcAndMoveRec 1 game
        |> GameTree.getMoves

    [<Benchmark>]
    member _.CalculateMovesDepth2AndMakeMove() = 
        GameTree.calcAndMoveRec 2 game

BenchmarkRunner.Run<GameBranchesBenchmarking> ()
|> ignore