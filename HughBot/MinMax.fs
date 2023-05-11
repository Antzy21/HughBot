module MinMax

open Chess
open SearchAlgorithms

/// From a gievn game, get the moves available and calculate the heuristic value if the move is applied.
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

let private printEval gameState depth move staticEvaluation =
    MoveParser.FullNotation.toString gameState.board move
    |> sprintf "%s Turn: %d %s - Eval : %.2f - Move: %s"
        (String.replicate depth "  ") (gameState.fullMoveClock)
        (gameState.playerTurn.ToString()) staticEvaluation 
    |> printfn "%s"

let private getNodesFromParent (gameState: gameState) =
    getMovesAndNewGameStates gameState
    |> orderMovesAndGsByStaticEval (Colour.toBool gameState.playerTurn)

let private evaluationFunction move gameState = Heuristics.staticEvaluationOfGameState gameState

let private chessMinMax = Algorithms.minMaxAbPruning getNodesFromParent evaluationFunction

let evaluation (game: game) : move option * float =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let isMaxing = Colour.toBool game.gameState.playerTurn
    let move, eval = chessMinMax 2 isMaxing None game.gameState
    Option.iter (
        MoveParser.FullNotation.toString game.gameState.board >> printfn "\n\nEval : %.2f - Move: %s" eval
    ) move
    
    stopWatch.Stop()
    printfn "Time taken: %.2f" stopWatch.Elapsed.TotalSeconds

    move, eval