module HughBot.PlayGame

open System
open System.IO
open FSharp.Extensions
open Chess

type Computer =
    {
        evaluationFunction: game -> move option * float;
    }

type PlayerType =
    | Human
    | Computer of Computer

let private getUserInputMoveFromNotation (game: gameState) : move option =
    Console.ParseLineWithBreakOption "Please enter move" (fun (notation: string) ->
        MoveParser.tryParse game.playerTurn game.board notation
        |> Result.toOption
    )
let private getUserInputMoveFromList (game: game) =
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
                getUserInputMoveFromNotation game.gameState
                |> Option.defaultWith (fun () ->
                    getUserInputMoveFromList game
                )
        stopWatch.Stop()
        MoveParser.AlgebraicNotation.toString move game.gameState.board |> (printf "%s")
        printfn $"\nTime taken: %.2f{stopWatch.Elapsed.TotalSeconds}"
        move

let play (fenOption: string) (depth: int) (colourString: string) (opponentString: string) (orderedEvaluation: bool) =
    printfn "Let's play!"

    let colour = Colour.tryParse colourString |> Option.get
    
    let hughbotComputer = Computer { evaluationFunction = MinMax.evaluation depth orderedEvaluation }    
    let hughbot = { name = "HughBot"; playerType = hughbotComputer; colour = colour }
            
    let opponent =
        match opponentString with
        | null -> 
            Console.WriteLine "I will play against myself!"
            { name = "HughBot"; playerType = hughbotComputer; colour = Colour.opposite colour }
        | opponent -> 
            Console.WriteLine $"Good luck {opponent}!"
            { name = opponent; playerType = Human; colour = Colour.opposite colour } 
                
    let date = DateTime.Now.ToString("yyyyMMdd")
    let fileName = 
        $"HughBot({Colour.toChar hughbot.colour})vs({Colour.toChar opponent.colour}){opponent.name}"
    let dir = $"{Environment.SpecialFolder.LocalApplicationData}/Antzy21/HughBot/RecordedGames"
    Directory.CreateDirectory(dir) |> ignore
    let path = $"{dir}/{fileName}_{date}.log"
    let file = File.CreateText(path)

    let mutable game =
        match fenOption with
        | null -> Game.Create.newGame ()
        | fen -> Game.Create.fromFen fen
    
    while Game.isGameOver game |> not do
        let currentMove =
            if game.gameState.playerTurn = hughbot.colour then
                hughbot.getMove game
            else
                opponent.getMove game
                
        file.WriteLine($"{MoveParser.FullNotation.toString game.gameState.board currentMove}")
        game <- Game.Update.makeMove currentMove game
        GameState.print game.gameState
        GameState.toFen game.gameState |> printfn "%s"
    
    file.WriteLine(GameState.toFen game.gameState)
    file.Close()

    printfn("\n==========\nGame Moves\n==========")
    printfn $"%s{Game.pgn game}"

    printfn "\nGood game!"

let evaluatePosition (fen: string) (depth: int) (orderedEvaluation: bool) =
    try
        let game = Game.Create.fromFen fen
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        Game.print game
        printfn "\nCalculating move...\n"
            
        let hughbotComputer = { evaluationFunction = MinMax.evaluation depth orderedEvaluation }   
        let moveOption, eval = hughbotComputer.evaluationFunction game
        
        stopWatch.Stop()
        printfn $"Time taken: %.2f{stopWatch.Elapsed.TotalSeconds}"
        match moveOption with
        | Some move ->
            printfn $"{MoveParser.FullNotation.toString game.gameState.board move}\n{eval}"        
        | None ->
            printfn $"No Move\n{eval}"
    with
    | _ -> ()
