module MinMax

open Chess

type minMaxConfig = {maxDepth: int; shallowTrim: int; deepTrim: int; deepTrimDepth: int;}

/// From a gievn game, get the moves available and calculate the heuristic value if the move is applied.
let private getMovesAndEvaluationPairs (game: game) : (float * move) list =
    GameState.getMoves game.gameState
    |> List.map (fun move ->
        let newGs = Game.Update.makeMove move game
        let eval = Heuristics.staticEvaluationOfGameState newGs.gameState
        eval, move
    )

/// Orders the list of moves based on evaluation desired by player of move
let private orderMoves (game: game) (moveEvalPairs: (float * move) list) : (float * move) list =
    match game.gameState.playerTurn with
    | White -> List.sortByDescending fst moveEvalPairs
    | Black -> List.sortBy fst moveEvalPairs

let private trimMoveList (depth: int) (config: minMaxConfig) (moveEvalPairs: (float * move) list) : (float * move) list =
    let trim =
        if depth >= config.deepTrimDepth then config.deepTrim
        else config.shallowTrim
    List.take (min moveEvalPairs.Length trim) moveEvalPairs

let private printEval game depth move staticEvaluation =
    MoveParser.FullNotation.toString game.gameState.board move
    |> sprintf "%s Turn: %d %s - Eval : %.2f - Move: %s"
        (String.replicate depth "  ") (game.gameState.fullMoveClock)
        (game.gameState.playerTurn.ToString()) staticEvaluation 
    |> printfn "%s"

let rec private minMaxEvaluation (config: minMaxConfig) (depth: int) (game: game) : move option * float =
    if depth = config.maxDepth then
        None, Heuristics.staticEvaluationOfGameState game.gameState
    else if GameState.getMoves game.gameState = List.empty then
        None, Heuristics.gameOverEvaluation depth game.gameState
    else
        let orderedTrimmedMovesAndEvals =
            getMovesAndEvaluationPairs game
            |> orderMoves game
            |> trimMoveList depth config

        let movesAndEvals =
            if depth = 0 then
                printf "Total moves: %d" orderedTrimmedMovesAndEvals.Length
                orderedTrimmedMovesAndEvals
                |> List.map (fun (staticEvaluation, move) ->
                    printEval game depth move staticEvaluation
                    let asyncTask = async {
                        let newGs = Game.Update.makeMove move game
                        let evaluation =
                            newGs
                            |> minMaxEvaluation config (depth + 1)
                            |> snd
                        return (Some move, evaluation)
                    }
                    Async.StartAsTask(asyncTask)
                )
                |> List.map (fun task ->
                    let returnValue =
                        Async.AwaitTask task
                        |> Async.RunSynchronously
                    printfn "Completed"
                    returnValue
                )
            else
                orderedTrimmedMovesAndEvals
                |> List.map (fun (staticEvaluation, move) ->
                    if depth < 2 then
                        printEval game depth move staticEvaluation
                    let newGs = Game.Update.makeMove move game
                    let evaluation = 
                        newGs
                        |> minMaxEvaluation config (depth + 1)
                        |> snd
                    (Some move, evaluation)
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
    let config = {maxDepth = 4; shallowTrim = 50; deepTrim = 4; deepTrimDepth = 2}
    let move, eval = minMaxEvaluation config 0 game
    Option.iter (
        MoveParser.FullNotation.toString game.gameState.board >> printfn "\n\nEval : %.2f - Move: %s" eval
    ) move
    
    stopWatch.Stop()
    printfn "Time taken: %.2f" stopWatch.Elapsed.TotalSeconds

    move, eval