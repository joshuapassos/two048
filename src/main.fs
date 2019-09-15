open System

let board = [[0 ; 0 ; 0 ; 0; 0] ; [0 ; 0 ; 0 ; 0; 0]; [0 ; 0 ; 0 ; 0; 0]; [0 ; 0 ; 0 ; 0; 0]; [0 ; 0 ; 0; 0; 0]]
let mutable x = board
let mutable test = false
let mutable score = 0
let rec leftLineAction aList =
    match aList with
    | [] -> []
    | [x] -> [x]
    | 0 :: z :: xs -> leftLineAction (z :: xs) @ [0] 
    | x :: 0 :: xs -> leftLineAction (x :: xs) @ [0] 
    | x :: z :: xs ->
        if x = z then
            score <- if not test then score + x + z else score
            (x + z) :: leftLineAction (xs @ [0]) 
        else
            x :: leftLineAction (z :: xs) 

let leftAction aList = aList |> List.map leftLineAction
let rightAction aList = (leftAction (aList |> List.map List.rev)) |> List.map List.rev 
let upAction aList = List.transpose (leftAction (List.transpose aList))
let downAction aList = List.transpose ((leftAction ((List.transpose aList) |> List.map List.rev)) |> List.map List.rev)

let createBoard(board: int list list) =
    board |> List.map (fun x ->
        x |> List.map (fun y -> 
            let r = System.Random()
            let num = if y = 0 then (if r.NextDouble() < 0.75 then 0 elif r.NextDouble() < 0.9 then 2 else 4) else y 
            num
        )
    )

let printBoard board =
    printfn "SCORE: %i " score
    (List.replicate 5 "_____") |> Seq.iter (fun x -> printf "%s " x)
    printfn "\n"
    for line in board do
        for element in line do
            printf "| %i " element
        printf "\n"

let gameOver x =
    System.Console.Clear()
    test <- true
    if x = upAction x && x = downAction x && x = leftAction x && x = rightAction x then
        printf "GAME OVER :< "
        printBoard x
        System.Environment.Exit(0)
    else
        printBoard x
    test <- false
    
let moveBoard key =
    match key with
    | ConsoleKey.UpArrow ->
        x <- upAction x
    | ConsoleKey.DownArrow ->
        x <- downAction x
    | ConsoleKey.LeftArrow ->
        x <- leftAction x
    | ConsoleKey.RightArrow ->
        x <- rightAction x
    | _ ->
        x <- board
        score <- 0
    x <- createBoard x
    printBoard x
    gameOver x

moveBoard ConsoleKey.N 
while (true) do
    Threading.Thread.Sleep(1);
    let key = Console.ReadKey().Key
    moveBoard key
