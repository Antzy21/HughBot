module MinMax

open Chess

type minMaxConfig = {maxDepth: int; shallowTrim: int; deepTrim: int; deepTrimDepth: int;}

let rec private minMaxEvaluation (config: minMaxConfig) (depth: int) (game: game) : move option * float =
    if depth = config.maxDepth then
        None, Heuristics.staticEvaluationOfGameState game.gameState
    else if GameState.getMoves game.gameState = List.empty then
        None, Heuristics.gameOverEvaluation depth game.gameState
    else
        let ms = GameState.getMoves game.gameState

        let newGsStaticEvals = 
            List.map (fun move ->
                let newGs = Game.Update.makeMove move game
                let eval = Heuristics.staticEvaluationOfGameState newGs.gameState
                //Board.print game.gameState.board
                //System.Console.Read () |> ignore
                Game.Update.undoMove newGs |> ignore
                eval, move
            ) ms

        let orderedEvals = 
            newGsStaticEvals |>
            match game.gameState.playerTurn with
            | White -> List.sortByDescending fst
            | Black -> List.sortBy fst
        
        let trim =
            if depth >= config.deepTrimDepth then config.deepTrim
            else config.shallowTrim

        let newGsStaticEval = List.take (min orderedEvals.Length trim) orderedEvals

        let printEval move staticEvaluation =
            MoveParser.FullNotation.toString move
            |> printfn "%s Turn: %d %s - Eval : %.2f - Move: %s"
                (String.replicate depth "  ") (game.gameState.fullMoveClock)
                (game.gameState.playerTurn.ToString()) staticEvaluation

        let movesAndEvals =
            newGsStaticEval
            |> List.map (fun (staticEvaluation, move) ->
                if depth < 2 then
                    printEval move staticEvaluation
                let newGs = Game.Update.makeMove move game
                let evaluation = 
                    newGs
                    |> minMaxEvaluation config (depth + 1)
                    |> snd
                Game.Update.undoMove newGs |> ignore
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
    let config = {maxDepth = 4; shallowTrim = 4; deepTrim = 2; deepTrimDepth = 4}
    let move, eval = minMaxEvaluation config 0 game
    Option.iter (
        MoveParser.FullNotation.toString >> printfn "\n\nEval : %.2f - Move: %s" eval
    ) move
    
    stopWatch.Stop()
    printfn "Time taken: %.2f" stopWatch.Elapsed.TotalSeconds

    move, eval