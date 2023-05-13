namespace HughBot.Benchmarking

open Chess

type GameTree =
    | Branches of GameTree list
    | Game of game
    | Moves of move list

module GameTree =

    let rec private calcAndMoveRec (getMovesFunc: gameState -> move list) (makeMovesFunc)
        (depth: int) (finishWithMoves: bool) (game: game) =
        if depth <= 0 then
            if finishWithMoves then
                Moves (getMovesFunc game.gameState)
            else
                Game game
        else
            getMovesFunc game.gameState
            |> List.map (fun move ->
                makeMovesFunc move game 
            )
            |> List.map (calcAndMoveRec getMovesFunc makeMovesFunc (depth-1) finishWithMoves)
            |> Branches

    let asyncBranchChessGameState = calcAndMoveRec GameState.getMovesAsync Game.Update.makeMove

    let branchChessGameState = calcAndMoveRec GameState.getMoves Game.Update.makeMove
