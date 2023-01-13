module Heuristics

open System
open Chess
open Checkerboard
open FSharp.Extensions

let private getBaseValue (square: square) : float option =
    square
    |> Square.getPiece
    |> Piece.getValue
    |> Option.map float
    
let private pieceFlexibilityValue (board: board) (square: square) : float =
    Board.GetSquares.pieceVision square board
    |> List.length |> float

let centralityBonusOnLine (i: sbyte) : float =
    3.5-(abs((float i) - 3.5))

let private centralityBonus (board: board) ((i, j): coordinates<sbyte>) : float =
    centralityBonusOnLine i + centralityBonusOnLine j

let private pawnAdvanceValue (square: square) : float =
    let piece = Square.getPiece square
    let (i,j) = square.coordinates
    let centralityBonus = centralityBonusOnLine i
    match piece.colour with
    | White -> 
        (float j) - 1.
    | Black ->
        6. - (float j)
    |> (*) (1. + centralityBonus*0.2)
let private staticValueOfSquareOnBoard (board: board) (square: square) : float option =
    match square.piece with
    | None -> None
    | Some piece ->
        let baseValue = getBaseValue square
        let flexValue =
            match piece.pieceType with
            | Knight -> 
                centralityBonus board square.coordinates
                |> (*) 0.01  |> Some
            | Bishop -> 
                centralityBonus board square.coordinates
                |> (*) (pieceFlexibilityValue board square)
                |> (*) 0.002 |> Some
            | Queen -> 
                centralityBonus board square.coordinates
                |> (*) (pieceFlexibilityValue board square)
                |> (*) 0.0001 |> Some
            | Rook -> 
                centralityBonus board square.coordinates
                |> (*) (pieceFlexibilityValue board square)
                |> (*) 0.002 |> Some
            | Pawn -> pawnAdvanceValue square |> (*) 0.005 |> Some
            | King -> None
        Option.map2 (+) baseValue flexValue
        |> Option.map (fun value ->
            match piece.colour with
            | White -> 1.
            | Black -> -1.
            |> (*) value 
        )

let gameOverEvaluation (turnsUntillGameOver: int) (game: gameState) : float =
    if Board.isInCheck game.playerTurn game.board then
        match game.playerTurn with
        | White -> -1000 + turnsUntillGameOver
        | Black -> 1000 - turnsUntillGameOver
    else
        0
    |> float

let staticEvaluationOfGameState (game: gameState) : float =
    if GameState.getMoves game = List.empty then
        gameOverEvaluation 0 game
    else
        game.board
        |> Array2D.fold (fun boardValue square ->
            match staticValueOfSquareOnBoard game.board square with
            | None -> boardValue
            | Some value -> boardValue + value
        ) 0

let printEvalutation (game: gameState) =
    printfn "Static heuristics evaluation: %.3f" (staticEvaluationOfGameState game)