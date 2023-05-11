module MinMax

open Chess
open SearchAlgorithms

/// From a given game, get the moves available and calculate the heuristic value if the move is applied.
let private getMovesAndNewGameStates (gameState: gameState) : (move * gameState) list =
    GameState.getMoves gameState
    |> List.map (fun move ->
        let newGs = GameState.Update.makeMove move gameState
        move, newGs
    )

/// Orders the list of moves based on evaluation desired by player of move
let private orderMovesAndGsByStaticEval (isMaxing: bool) : (move * gameState) list -> (move * gameState) list =
    fun (move, gameState) -> Heuristics.staticEvaluationOfGameState gameState
    |>
    if isMaxing then
        List.sortByDescending
    else
        List.sortBy

let private getNodesFromParent (gameState: gameState) =
    getMovesAndNewGameStates gameState
    //|> orderMovesAndGsByStaticEval (Colour.toBool gameState.playerTurn)

let private evaluationFunction move gameState = Heuristics.staticEvaluationOfGameState gameState

let private chessMinMax = Algorithms.minMaxAbPruning getNodesFromParent evaluationFunction

let evaluation (game: game) : move option * float =
    
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let depth = 2
    let isMaxing = Colour.toBool game.gameState.playerTurn
    let move, eval = chessMinMax depth isMaxing (Some game.moves.Head) game.gameState
    
    Option.iter (
        MoveParser.FullNotation.toString game.gameState.board
        >> printfn "\n\nEval : %.2f - Move: %s" eval
    ) move
    
    stopWatch.Stop()
    printfn "Time taken: %.2f" stopWatch.Elapsed.TotalSeconds

    move, eval