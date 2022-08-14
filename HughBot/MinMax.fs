module MinMax

open Chess

let rec private minMaxEvaluation (maxDepth: int) (depth: int) (shallowTrim: int) (deepTrim: int) (deepTrimDepth: int) (game: gameState) : move option * float =
    if depth = maxDepth then
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
            if depth >= deepTrimDepth then deepTrim
            else shallowTrim

        let newGsStaticEval = List.take (min newGsStaticEval.Length trim) newGsStaticEval

        let movesAndEvals =
            newGsStaticEval
            |> List.map snd
            |> List.map (fun move ->
                let evaluation = 
                    GameState.makeMove move game
                    |> minMaxEvaluation maxDepth shallowTrim deepTrim deepTrimDepth (depth + 1)
                    |> snd
                (Some move, evaluation)
            )

        let bestMoveAndEval = 
            movesAndEvals |>
            match game.playerTurn with
            | White -> List.sortByDescending snd
            | Black -> List.sortBy snd
            |> List.head

        if depth < 3 then
            let move, eval = bestMoveAndEval
            Option.iter ( 
                Move.getMoveNotation
                >> printfn "%s Turn: %s - Eval : %.2f - Move: %s" (String.replicate depth " ") (game.playerTurn.ToString()) eval
            ) move

        bestMoveAndEval

let evaluation (depth: int) (game: gameState) : move option * float =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let move, eval = minMaxEvaluation depth 4 2 3 0 game
    Option.iter (
        Move.getMoveNotation >> printfn "\n\nEval : %.2f - Move: %s" eval
    ) move
    
    stopWatch.Stop()
    printfn "Time taken: %.2f" stopWatch.Elapsed.TotalSeconds

    move, eval