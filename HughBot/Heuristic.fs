module Heuristic

open Chess
open Checkerboard
open FSharp.Extensions

let private getBaseValue (square: square) : float option =
    square
    |> Square.getPiece
    |> Piece.getValue
    |> Option.map float

let private staticValueOfKnight (square: square) : float =
    let (i, j) = square.coordinates
    let baseValue = getBaseValue square |> Option.get
    baseValue +
    if i>1 && i<6 then
        0.02
    elif i>0 && i<7 then
        0.01
    else
        0
    +
    if j>1 && j<6 then
        0.02
    elif j>0 && j<7 then
        0.01
    else
        0
    
let private pieceFlexibility (board: board) (square: square) : float =
    Square.getMoves board square
    |> List.length |> float
    |> (*) 0.01

let private staticValueOfPiece (board: board) (square: square) : float =
    getBaseValue square
    |> Option.get
    |> (+) <| pieceFlexibility board square

let private staticValueOfPawn (square: square) : float =
    0
    
let private staticValueOfKing (square: square) : float =
    0

let private staticValueOfSquareOnBoard (board: board) (square: square) : float =
    match square.piece with
    | None -> 0
    | Some piece ->
        match piece.pieceType with
        | Knight -> staticValueOfKnight square
        | Bishop -> staticValueOfPiece board square
        | Queen -> staticValueOfPiece board square
        | Rook -> staticValueOfPiece board square
        | Pawn -> staticValueOfPawn square
        | King -> staticValueOfKing square

let staticValueOfGameState (game: gameState) : float =
    game.board
    |> Array2D.fold (fun boardValue square ->
        boardValue + staticValueOfSquareOnBoard game.board square
    ) 0