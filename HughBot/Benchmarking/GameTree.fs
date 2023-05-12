namespace HughBot.Benchmarking

open Chess

type GameTree =
    | Branches of GameTree list
    | Game of game
    | Moves of move list

module GameTree =

    let calcAndMove (game: game) =
        GameState.getMoves game.gameState
        |> List.map (fun move ->
            Game.Update.makeMove move game 
        )

    let rec calcAndMoveRec (depth: int) (finishWithMoves: bool) (game: game) =
        if depth <= 0 then
            if finishWithMoves then
                Moves (GameState.getMoves game.gameState)
            else
                Game game
        else
            calcAndMove game
            |> List.map (calcAndMoveRec (depth-1) finishWithMoves)
            |> Branches

