module Heuristics

open Chess
open Checkerboard
open FSharp.Extensions

let private getBaseValue (square: square) : float option =
    square
    |> Square.getPiece
    |> Piece.getValue
    |> Option.map float

let private knightCentralityValue (square: square) : float =
    let (i, j) = square.coordinates
    if i>1 && i<6 then 2.
    elif i>0 && i<7 then 1.
    else 0.
    +
    if j>1 && j<6 then 2.
    elif j>0 && j<7 then 1.
    else 0.
    
let private pieceFlexibilityValue (board: board) (square: square) : float =
    Square.getMoves board square
    |> List.length |> float

let private pawnAdvanceValue (square: square) : float =
    let piece = Square.getPiece square
    let (i,j) = square.coordinates
    let centralityBonus = 3.5-(abs((float i) - 3.5))
    match piece.colour with
    | White -> 
        (float j) - 1.
    | Black ->
        6. - (float j)
    |> (*) (1. + centralityBonus*0.1)
let private staticValueOfSquareOnBoard (board: board) (square: square) : float option =
    match square.piece with
    | None -> None
    | Some piece ->
        let baseValue = getBaseValue square
        let flexValue =
            match piece.pieceType with
            | Knight -> knightCentralityValue square |> (*) 0.01  |> Some
            | Bishop -> pieceFlexibilityValue board square |> (*) 0.002 |> Some
            | Queen -> pieceFlexibilityValue board square |> (*) 0.0001 |> Some
            | Rook -> pieceFlexibilityValue board square |> (*) 0.002 |> Some
            | Pawn -> pawnAdvanceValue square |> (*) 0.02 |> Some
            | King -> None
        Option.map2 (+) baseValue flexValue
        |> Option.map (fun value ->
            match piece.colour with
            | White -> 1.
            | Black -> -1.
            |> (*) value 
        )

let staticEvaluationOfGameState (game: gameState) : float =
    game.board
    |> Array2D.fold (fun boardValue square ->
        match staticValueOfSquareOnBoard game.board square with
        | None -> boardValue
        | Some value -> boardValue + value
    ) 0

let printEvalutation (game: gameState) =
    printfn "Static heuristics evaluation: %.3f" (staticEvaluationOfGameState game)