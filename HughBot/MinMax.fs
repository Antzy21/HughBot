module MinMax

open Chess
open SearchAlgorithms

/// From a gievn game, get the moves available and calculate the heuristic value if the move is applied.
let private getMovesAndEvaluationPairs (gameState: gameState) : moveAndEvaluation<move, float> list =
    GameState.getMoves gameState
    |> List.map (fun move ->
        let newGs = GameState.Update.makeMove move gameState
        let eval = Heuristics.staticEvaluationOfGameState newGs
        {move = Some move; eval = eval}
    )

/// Orders the list of moves based on evaluation desired by player of move
let private orderMoves (gameState: gameState) (moveEvalPairs: moveAndEvaluation<move, float> list) : moveAndEvaluation<move, float> list =
    match gameState.playerTurn with
    | White -> moveEvalPairs |> List.sortByDescending (fun mep -> mep.eval)
    | Black -> moveEvalPairs |> List.sortByDescending (fun mep -> mep.eval)

let private printEval gameState depth move staticEvaluation =
    MoveParser.FullNotation.toString gameState.board move
    |> sprintf "%s Turn: %d %s - Eval : %.2f - Move: %s"
        (String.replicate depth "  ") (gameState.fullMoveClock)
        (gameState.playerTurn.ToString()) staticEvaluation 
    |> printfn "%s"

let rec private minMaxEvaluation (depth: int) (gameState: gameState) : move option * float =
    if depth = 0 then
        None, Heuristics.staticEvaluationOfGameState gameState
    else if GameState.getMoves gameState = List.empty then
        None, Heuristics.gameOverEvaluation depth gameState
    else
        let orderedTrimmedMovesAndEvals =
            getMovesAndEvaluationPairs gameState
            |> orderMoves gameState

        let movesAndEvals =
            orderedTrimmedMovesAndEvals
            |> List.map (fun moveAndEval ->
                if depth > 1 then
                    printEval gameState depth moveAndEval.move.Value moveAndEval.eval
                let newGs = GameState.Update.makeMove moveAndEval.move.Value gameState
                let evaluation = 
                    newGs
                    |> minMaxEvaluation (depth - 1)
                    |> snd
                (moveAndEval.move, evaluation)
            )

        let bestMoveAndEval = 
            movesAndEvals |>
            match gameState.playerTurn with
            | White -> List.sortByDescending snd
            | Black -> List.sortBy snd
            |> List.head


        bestMoveAndEval

let evaluation (game: game) : move option * float =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let move, eval = minMaxEvaluation 4 game.gameState
    Option.iter (
        MoveParser.FullNotation.toString game.gameState.board >> printfn "\n\nEval : %.2f - Move: %s" eval
    ) move
    
    stopWatch.Stop()
    printfn "Time taken: %.2f" stopWatch.Elapsed.TotalSeconds

    move, eval