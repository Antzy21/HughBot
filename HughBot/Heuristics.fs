module Heuristics

open System
open Chess
open Checkerboard
open FSharp.Extensions

let private getBaseValue (square: square) : float option =
    square.Value
    |> Piece.getValue
    |> Option.map float
    
let private pieceFlexibilityValue (board: board) (coords: coordinates) : float =
    Board.GetSquares.pieceVision board coords
    |> List.length |> float

let centralityBonusOnLine (i: int) : float =
    3.5-(abs((float i) - 3.5))

let private centralityBonus (board: board) ((i, j): coordinates) : float =
    centralityBonusOnLine i + centralityBonusOnLine j

let private pawnAdvanceValue (sqr: squareBitMap) ((i,j): coordinates) : float =
    let centralityBonus = centralityBonusOnLine i
    let colour = Square.getPieceColour sqr |> Option.get
    match colour with
    | White -> 
        (float j) - 1.
    | Black ->
        6. - (float j)
    |> (*) (1. + centralityBonus*0.2)
let private staticValueOfSquareOnBoard (board: board) (coords: coordinates) (sqr: squareBitMap) : float option =
    match Square.Parser.fromBitMaps sqr with
    | None -> None
    | Some piece ->
        let baseValue =
            Board.GetSquare.fromCoordinates board coords
            |> Square.Parser.fromBitMaps
            |> getBaseValue
        let flexValue =
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
            | Pawn -> pawnAdvanceValue sqr coords |> (*) 0.005 |> Some
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
        |> Board.foldij (fun coords boardValue sqr ->
            match staticValueOfSquareOnBoard game.board coords sqr with
            | None -> boardValue
            | Some value -> boardValue + value
        ) 0

let printEvalutation (game: gameState) =
    printfn "Static heuristics evaluation: %.3f" (staticEvaluationOfGameState game)