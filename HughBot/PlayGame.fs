module PlayGame

open FSharp.Extensions
open Chess

    
let play (gs: gameState) =
    
    let mutable game = gs
    printfn "I'm HughBot, a chess engine. Let's play!"
    let botColour = Console.ParseLine "Please select White or Black for bot to play" Colour.tryParse 

    let mutable moveList : string list = []

    while GameState.isGameOver game |> not do
        let currentMove =
            if game.playerTurn = botColour then
                printfn "Calculating move..."
                let move, _ = MinMax.evaluation 5 game
                move |> Option.get
            else
                let moves = GameState.getMovesForPlayer game
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
        moveList <- List.append moveList [Move.getMoveNotation currentMove]
        game <- GameState.makeMove currentMove game
        GameState.print game
        GameState.toFen game |> printfn "%s"

    List.iter (printfn "%s") moveList

    printfn "Good game!"

let newGame () =
    GameState.newGame () 
    |> play

let playFromFen (fen : string) =
    GameState.fromFen fen
    |> play