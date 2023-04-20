module MinMax

open Chess
open SearchAlgorithms

/// From a gievn game, get the moves available and calculate the heuristic value if the move is applied.
let private getMovesAndEvaluationPairs (game: game) : moveAndEvaluation<move, float> list =
    GameState.getMoves game.gameState
    |> List.map (fun move ->
        let newGs = Game.Update.makeMove move game
        let eval = Heuristics.staticEvaluationOfGameState newGs.gameState
        {move = Some move; eval = eval}
    )

/// Orders the list of moves based on evaluation desired by player of move
let private orderMoves (game: game) (moveEvalPairs: moveAndEvaluation<move, float> list) : moveAndEvaluation<move, float> list =
    match game.gameState.playerTurn with
    | White -> moveEvalPairs |> List.sortByDescending (fun mep -> mep.eval)
    | Black -> moveEvalPairs |> List.sortByDescending (fun mep -> mep.eval)

let private printEval game depth move staticEvaluation =
    MoveParser.FullNotation.toString game.gameState.board move
    |> sprintf "%s Turn: %d %s - Eval : %.2f - Move: %s"
        (String.replicate depth "  ") (game.gameState.fullMoveClock)
        (game.gameState.playerTurn.ToString()) staticEvaluation 
    |> printfn "%s"

let rec private minMaxEvaluation (depth: int) (game: game) : move option * float =
    if depth = 0 then
        None, Heuristics.staticEvaluationOfGameState game.gameState
    else if GameState.getMoves game.gameState = List.empty then
        None, Heuristics.gameOverEvaluation depth game.gameState
    else
        let orderedTrimmedMovesAndEvals =
            getMovesAndEvaluationPairs game
            |> orderMoves game

        let movesAndEvals =
            orderedTrimmedMovesAndEvals
            |> List.map (fun moveAndEval ->
                if depth > 1 then
                    printEval game depth moveAndEval.move.Value moveAndEval.eval
                let newGs = Game.Update.makeMove moveAndEval.move.Value game
                let evaluation = 
                    newGs
                    |> minMaxEvaluation (depth - 1)
                    |> snd
                (moveAndEval.move, evaluation)
            )

        let bestMoveAndEval = 
            movesAndEvals |>
            match game.gameState.playerTurn with
            | White -> List.sortByDescending snd
            | Black -> List.sortBy snd
            |> List.head


        bestMoveAndEval

let evaluation (game: game) : move option * float =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let move, eval = minMaxEvaluation 4 game
    Option.iter (
        MoveParser.FullNotation.toString game.gameState.board >> printfn "\n\nEval : %.2f - Move: %s" eval
    ) move
    
    stopWatch.Stop()
    printfn "Time taken: %.2f" stopWatch.Elapsed.TotalSeconds

    move, eval