module HughBot.MinMax

open Chess
open SearchAlgorithms

/// From a given game, get the moves available and calculate the heuristic value if the move is applied.
let private getMovesAndNewGameStates (game: game) : (move * game) list =
    GameState.getMoves game.gameState
    |> List.map (fun move ->
        let newGs = Game.Update.makeMove move game
        move, newGs
    )

/// Orders the list of moves based on evaluation desired by player of move
let private orderMovesAndGsByStaticEval (isMaxing: bool) : (move * game) list -> (move * game) list =
    fun (move, game) -> Heuristics.advancedEval game
    |>
    if isMaxing then
        List.sortByDescending
    else
        List.sortBy

let private getNodesFromParent (game: game) =
    getMovesAndNewGameStates game
    //|> orderMovesAndGsByStaticEval (Colour.toBool gameState.playerTurn)

let private evaluationFunction move game = Heuristics.advancedEval game

let private chessMinMax = Algorithms.minMaxAbPruning getNodesFromParent evaluationFunction

let evaluation (game: game) : move option * float =
    let depth = 3
    let isMaxing = Colour.toBool game.gameState.playerTurn
    let previousMove = 
        match game.moves with
        | [] -> None
        | lastMove :: _ -> Some lastMove
    chessMinMax depth isMaxing previousMove game