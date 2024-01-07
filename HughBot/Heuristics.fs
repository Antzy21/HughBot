module HughBot.Heuristics

open Chess
open Checkerboard
open FSharp.Extensions
    
let private pieceFlexibilityValue (board: board) (coords: coordinates) : float =
    Board.Vision.ofPieceAtCoords board coords
    |> Result.failOnError
    |> BitMap.isolateValues
    |> List.length |> float

let private centralityBonusOnLine (i: int) : float =
    3.5-(abs((float i) - 3.5))

let private centralityBonus (board: board) (c: coordinates) : float =
    let fileBonus = 
        Coordinates.getFile c
        |> centralityBonusOnLine 
    let rowBonus = 
        Coordinates.getRow c
        |> centralityBonusOnLine
    fileBonus + rowBonus

let private pawnAdvanceValue (piece: piece) (c: coordinates) : float =
    let i = Coordinates.getFile c
    let j = Coordinates.getRow c
    let centralityBonus = centralityBonusOnLine i
    match piece.colour with
    | White -> 
        (float j) - 1.
    | Black ->
        6. - (float j)
    |> (*) (1. + centralityBonus*0.2)

let private calculateFlexValue (piece: piece) (board: board) (coords: coordinates) : float option =
    match piece.pieceType with
        | Knight -> 
            centralityBonus board coords
            |> (*) 0.01  |> Some
        | Bishop -> 
            centralityBonus board coords
            |> (*) (pieceFlexibilityValue board coords)
            |> (*) 0.002 |> Some
        | Queen -> 
            centralityBonus board coords
            |> (*) (pieceFlexibilityValue board coords)
            |> (*) 0.0001 |> Some
        | Rook -> 
            centralityBonus board coords
            |> (*) (pieceFlexibilityValue board coords)
            |> (*) 0.002 |> Some
        | Pawn -> pawnAdvanceValue piece coords |> (*) 0.005 |> Some
        | King -> None

let private basicPieceValueEvaluationOfSquare (board: board) (coords: coordinates) (sqr: square) : float option =
    match sqr with
    | None -> None
    | Some piece ->
        let baseValue = Piece.getValue piece |> Option.map float
        baseValue
        |> Option.map (fun value ->
            match piece.colour with
            | White -> 1.
            | Black -> -1.
            |> (*) value 
        )

let private advancedEvaluationOfSquare (board: board) (coords: coordinates) (sqr: square) : float option =
    match sqr with
    | None -> None
    | Some piece ->
        let baseValue = Piece.getValue piece |> Option.map float
        let flexValue = calculateFlexValue piece board coords
        Option.map2 (+) baseValue flexValue
        |> Option.map (fun value ->
            match piece.colour with
            | White -> 1.
            | Black -> -1.
            |> (*) value 
        )

let private gameOverEvaluation (turnsUntilGameOver: int) (game: game) : float =
    if Board.isInCheck game.gameState.playerTurn game.gameState.board then
        match game.gameState.playerTurn with
        | White -> -1000 + turnsUntilGameOver
        | Black -> 1000 - turnsUntilGameOver
    else
        0
    |> float

// The general evaluation wrapper function.
// Just needs to know how to evaluate each individual square.
let private staticEvaluationOfGameState (squareEvalFunction: board -> coordinates -> square -> float option) (game: game) : float =
    if Game.isGameOver game then
        gameOverEvaluation 0 game
    else
        game.gameState.board
        |> Board.foldjiback (fun coords boardValue sqr ->
            match advancedEvaluationOfSquare game.gameState.board coords sqr with
            | None -> boardValue
            | Some value -> boardValue + value
        ) 0

let getComputations (squareEvalFunction) (board) (sqrList) =
    [
        for coords in sqrList do
            async {
                let value =
                    let sqr = Board.getSquareFromCoordinates board coords
                    match squareEvalFunction board coords sqr with
                    | Some value -> value
                    | None -> 0.
                return value
            }
    ]

let private allCoords =
    Seq.allPairs [0..7] [0..7]
    |> Seq.map (fun (x, y) -> 
        Coordinates.construct x y
        |> Result.failOnError
    )

let private asyncStaticEvalOfGameState (maxParallelism: int)
    (squareEvalFunction: board -> coordinates -> square -> float option)
    (game: game) : float =
    if Game.isGameOver game then
        gameOverEvaluation 0 game
    else
        allCoords
        |> getComputations squareEvalFunction game.gameState.board
        |> fun comp -> Async.Parallel(comp, maxParallelism)
        |> Async.StartAsTask
        |> (fun task ->
            task.Wait()
            task.Result
        )
        |> Array.reduce (+)

let pieceValueOnlyEval = staticEvaluationOfGameState basicPieceValueEvaluationOfSquare

let advancedEval = staticEvaluationOfGameState advancedEvaluationOfSquare

let asyncAdvancedEval maxParallelism = (asyncStaticEvalOfGameState maxParallelism) advancedEvaluationOfSquare