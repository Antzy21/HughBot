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
    fun (move, gameState) -> Heuristics.advancedEval gameState
    |>
    if isMaxing then
        List.sortByDescending
    else
        List.sortBy

let private getNodesFromParent (gameState: gameState) =
    getMovesAndNewGameStates gameState
    //|> orderMovesAndGsByStaticEval (Colour.toBool gameState.playerTurn)

let private evaluationFunction move gameState = Heuristics.advancedEval gameState

let private chessMinMax = Algorithms.minMaxAbPruning getNodesFromParent evaluationFunction

let evaluation (game: game) : move option * float =
    let depth = 2
    let isMaxing = Colour.toBool game.gameState.playerTurn
    let previousMove = 
        match game.moves with
        | [] -> None
        | lastMove :: _ -> Some lastMove
    chessMinMax depth isMaxing previousMove game.gameState