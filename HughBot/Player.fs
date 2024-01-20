namespace HughBot

open Chess
open FSharp.Extensions
open HughBot

type PlayerType =
    | Human
    | Computer of Computer

module private UserInput =
    
    let getMoveFromNotation (game: gameState) : move option =
        Console.ParseLineWithBreakOption "Please enter move" (fun (notation: string) ->
            MoveParser.tryParse game.playerTurn game.board notation
            |> Result.toOption
        )
        
    let getMoveFromList (game: game) =
        let moves = GameState.getMoves game.gameState
        moves |> List.iteri (fun i m -> printfn $"({i}) {MoveParser.FullNotation.toString game.gameState.board m}")
        Console.ParseLine "Please enter a valid move #" (fun (v: string) ->
            Int.tryParse v
            |> Option.bind (fun i ->
                if i < 0 || i > moves.Length then
                    None
                else
                    Some moves[i]
            )
        )

type Player =
    {name: string; playerType: PlayerType; colour: colour}
    member this.getMove (game: game) : move =
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let move =
            match this.playerType with
            | Computer c -> c.evaluationFunction game |> fst |> Option.get
            | Human ->
                UserInput.getMoveFromNotation game.gameState
                |> Option.defaultWith (fun () ->
                    UserInput.getMoveFromList game
                )
        stopWatch.Stop()
        MoveParser.AlgebraicNotation.toString move game.gameState.board |> (printf "%s")
        printfn $"\nTime taken: %.2f{stopWatch.Elapsed.TotalSeconds}"
        move
