module PlayGame

open FSharp.Extensions
open Chess
open System.IO

let private getUserInputMoveFromNotation (game: gameState) (moves: move list) : move option =
    Console.ParseLineWithBreakOption "Please enter move" (fun (notation: string) ->
        NotationParser.tryParse game.playerTurn game.board notation
    )
    
let private getUserInputMoveFromList (moves: move list) =
    moves |> List.iteri (fun i m -> printfn $"({i}) {Move.getMoveNotation m}")
    Console.ParseLine "Please enter a valid move #" (fun (v: string) ->
        Int.tryParse v
        |> Option.bind (fun i ->
            if i < 0 || i > moves.Length then
                None
            else
                Some moves[i]
        )
    )

let play (gs: gameState) =
    
    let mutable game = gs
    printfn "I'm HughBot, a chess engine. Let's play!"
    printfn "Who am I playing against? (Leave blank to test - No log will be created)"
    let opponent = System.Console.ReadLine ()
    let botColour = Console.ParseLine "Please select White or Black for bot to play" Colour.tryParse 
    
    let date = System.DateTime.Now.ToString("ddMMyyyy")
    let fileName = $"Hughbot({botColour |> Colour.toChar})vs({botColour |> Colour.opposite |> Colour.toChar}){opponent}_{date}.log"
    let path = $"../../../RecordedGames/{fileName}"
    let file = File.CreateText(path)

    let mutable moveList : string list = []

    while GameState.isGameOver game |> not do
        let currentMove =
            if game.playerTurn = botColour then
                printfn "Calculating move..."
                let move, _ =
                    try
                        MinMax.evaluation game
                    with
                    | ex ->
                        GameState.print game
                        file.WriteLine($"{ex}")
                        file.WriteLine($"{GameState.toFen game}")
                        file.Close()
                        failwith $"{ex}" 
                move |> Option.get
            else
                let moves = GameState.getMoves game
                getUserInputMoveFromNotation game moves
                |> Option.defaultWith (fun () -> getUserInputMoveFromList moves)
        file.WriteLine($"{Move.getMoveNotation currentMove}")
        moveList <- List.append moveList [Move.getMoveNotation currentMove]
        game <- GameState.Update.makeMove currentMove game
        GameState.print game
        GameState.toFen game |> printfn "%s"
    
    file.WriteLine(GameState.toFen game)
    file.Close()

    printfn("\n==========\nGame Moves\n==========")
    List.iter (printfn "%s") moveList

    printfn "\nGood game!"

    if opponent = "" then
        File.Delete path

let newGame () =
    GameState.Create.newGame () 
    |> play

let playFromFen (fen : string) =
    GameState.Create.fromFen fen
    |> play