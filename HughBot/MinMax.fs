module MinMax

open Chess

type minMaxConfig = {maxDepth: int; shallowTrim: int; deepTrim: int; deepTrimDepth: int;}

let rec private minMaxEvaluation (config: minMaxConfig) (depth: int) (game: gameState) : move option * float =
    if depth = config.maxDepth then
        None, Heuristics.staticEvaluationOfGameState game
    else if GameState.getMovesForPlayer game = List.empty then
        None, Heuristics.gameOverEvaluation depth game
    else
        let ms = GameState.getMovesForPlayer game

        let newGameStates = 
            List.map (fun move ->
                (GameState.makeMove move game, move)
            ) ms

        let newGsStaticEval = 
            newGameStates
            |> List.map (fun (game, move) ->
                (Heuristics.staticEvaluationOfGameState game, move)
            ) |>
            match game.playerTurn with
            | White -> List.sortByDescending fst
            | Black -> List.sortBy fst
        
        let trim =
            if depth >= config.deepTrimDepth then config.deepTrim
            else config.shallowTrim

        let newGsStaticEval = List.take (min newGsStaticEval.Length trim) newGsStaticEval

        let movesAndEvals =
            newGsStaticEval
            |> List.map (fun (staticEvaluation, move) ->
                Move.getMoveNotation move
                |> printfn "%s Turn: %d %s - Eval : %.2f - Move: %s"
                    (String.replicate depth "  ") (game.fullMoveClock)
                    (game.playerTurn.ToString()) staticEvaluation
                let evaluation = 
                    GameState.makeMove move game
                    |> minMaxEvaluation config (depth + 1)
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

let evaluation (game: gameState) : move option * float =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let config = {maxDepth = 3; shallowTrim = 2; deepTrim = 2; deepTrimDepth = 0}
    let move, eval = minMaxEvaluation config 0 game
    Option.iter (
        Move.getMoveNotation >> printfn "\n\nEval : %.2f - Move: %s" eval
    ) move
    
    stopWatch.Stop()
    printfn "Time taken: %.2f" stopWatch.Elapsed.TotalSeconds

    move, eval