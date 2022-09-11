module MinMax

open Chess

type minMaxConfig = {maxDepth: int; shallowTrim: int; deepTrim: int; deepTrimDepth: int;}

let rec private minMaxEvaluation (config: minMaxConfig) (depth: int) (game: gameState) : move option * float =
    if depth = config.maxDepth then
        None, Heuristics.staticEvaluationOfGameState game
    else if GameState.getMoves game = List.empty then
        None, Heuristics.gameOverEvaluation depth game
    else
        let ms = GameState.getMoves game

        let newGsStaticEvals = 
            List.map (fun move ->
                let newGs = GameState.Update.makeMove move game
                let eval = Heuristics.staticEvaluationOfGameState game
                GameState.Update.undoMove move game |> ignore
                eval, move
            ) ms

        let orderedEvals = 
            newGsStaticEvals |>
            match game.playerTurn with
            | White -> List.sortByDescending fst
            | Black -> List.sortBy fst
        
        let trim =
            if depth >= config.deepTrimDepth then config.deepTrim
            else config.shallowTrim

        let newGsStaticEval = List.take (min orderedEvals.Length trim) orderedEvals

        let movesAndEvals =
            newGsStaticEval
            |> List.map (fun (staticEvaluation, move) ->
                Move.getMoveNotation move
                |> printfn "%s Turn: %d %s - Eval : %.2f - Move: %s"
                    (String.replicate depth "  ") (game.fullMoveClock)
                    (game.playerTurn.ToString()) staticEvaluation
                let evaluation = 
                    GameState.Update.makeMove move game
                    |> minMaxEvaluation config (depth + 1)
                    |> snd
                GameState.Update.undoMove move game |> ignore
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
    let config = {maxDepth = 4; shallowTrim = 6; deepTrim = 2; deepTrimDepth = 2}
    let move, eval = minMaxEvaluation config 0 game
    Option.iter (
        Move.getMoveNotation >> printfn "\n\nEval : %.2f - Move: %s" eval
    ) move
    
    stopWatch.Stop()
    printfn "Time taken: %.2f" stopWatch.Elapsed.TotalSeconds

    move, eval