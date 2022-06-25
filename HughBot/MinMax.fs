module MinMax

open Chess

let rec private minMaxEvaluation (maxDepth: int) (depth: int) (game: gameState) : move option * float =
    if depth = maxDepth then
        None, Heuristics.staticEvaluationOfGameState game
    else if GameState.getMovesForPlayer game = List.empty then
        None, Heuristics.gameOverEvaluation depth game
    else
        let ms = GameState.getMovesForPlayer game

        let movesAndEvals =
            ms |> List.map (fun move ->
                let evaluation = 
                    GameState.makeMove move game
                    |> minMaxEvaluation maxDepth (depth + 1)
                    |> snd
                (Some move, evaluation)
            )

        let bestMoveAndEval = 
            movesAndEvals |>
            match game.playerTurn with
            | White -> List.sortByDescending snd
            | Black -> List.sortBy snd
            |> List.head

        bestMoveAndEval

let evaluation (depth: int) (game: gameState) : float =
    let move, eval = minMaxEvaluation depth 0 game
    Option.iter (Move.getMoveNotation >> printf "%s") move
    eval
