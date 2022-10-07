﻿module PlayGame

open System
open System.IO
open FSharp.Extensions
open Chess

type Opponent =
    | Test
    | Human of string
    | Computer

let private getUserInputForOpponent () : Opponent =
    Console.ParseLine "Who am I playing against? Leave blank to test, or 'HughBot'/'HB' to play myself!" (fun (oppString: string) ->
        match oppString.ToUpper() with
        | "HUGHBOT" | "HB" -> 
            Console.WriteLine "I will play against myself!"
            Some Computer
        | "" -> 
            Console.WriteLine "Test game."
            Some Test
        | _ -> 
            Console.WriteLine $"Good luck {oppString}!"
            Some (Human oppString)
    )

let private getUserInputMoveFromNotation (game: gameState) (moves: move list) : move option =
    Console.ParseLineWithBreakOption "Please enter move" (fun (notation: string) ->
        NotationParser.tryParse game.playerTurn game.board notation
    )
    
let private getUserInputMoveFromList (moves: move list) =
    moves |> List.iteri (fun i m -> printfn $"({i}) {Move.getFullNotation m}")
    Console.ParseLine "Please enter a valid move #" (fun (v: string) ->
        Int.tryParse v
        |> Option.bind (fun i ->
            if i < 0 || i > moves.Length then
                None
            else
                Some moves[i]
        )
    )

let private getComputerMove (game: game) (file: StreamWriter): move =
    printfn "\nCalculating move...\n"
    let move, _ =
        try
            MinMax.evaluation game
        with
        | ex ->
            Game.print game
            file.WriteLine($"{ex}")
            file.WriteLine($"{GameState.toFen game.gameState}")
            file.Close()
            failwith $"{ex}" 
    move |> Option.get

let private getOpponentMove (game: game) (opponent: Opponent) (file: StreamWriter): move =
    match opponent with
    | Computer -> getComputerMove game file
    | _ ->
        let moves = GameState.getMoves game.gameState
        getUserInputMoveFromNotation game.gameState moves
        |> Option.defaultWith (fun () -> getUserInputMoveFromList moves)

let play (game: game) =
    
    let mutable game = game
    printfn "I'm HughBot, a chess engine. Let's play!"
    let opponent = getUserInputForOpponent ()
    let botColour = 
        match opponent with
        | Computer -> White
        | _ -> Console.ParseLine "Please select White or Black for me to play" Colour.tryParse 
    
    let date = System.DateTime.Now.ToString("ddMMyyyy")
    let fileName = 
        match opponent with
        | Human name -> $"Hughbot({botColour |> Colour.toChar})vs({botColour |> Colour.opposite |> Colour.toChar}){name}"
        | Computer -> $"SelfPlay"
        | Test -> ""
    let path = $"../../../RecordedGames/{fileName}_{date}.log"
    let file = File.CreateText(path)

    while GameState.isGameOver game.gameState |> not do
        let currentMove =
            if game.gameState.playerTurn = botColour then
                getComputerMove game file
            else
                getOpponentMove game opponent file
        file.WriteLine($"{Move.getFullNotation currentMove}")
        game <- Game.Update.makeMove currentMove game
        Game.print game
        GameState.toFen game.gameState |> printfn "%s"
    
    file.WriteLine(GameState.toFen game.gameState)
    file.Close()

    printfn("\n==========\nGame Moves\n==========")
    printfn "%s" (Game.pgn game)

    printfn "\nGood game!"

    if opponent = Test then
        File.Delete path

let newGame () =
    Game.Create.newGame () 
    |> play

let playFromFen (fen : string) =
    {
        moves = []
        gameState = GameState.Create.fromFen fen
    }
    |> play