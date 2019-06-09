module DrawFunctions

    open System
    open DomainTypes
    open PieceFunctions
    open StateFunctions

    let boardTop = 4
    let boardLeft = 4
    let textOffset = 10
    let HintHeight = 4

    let private drawCell x y col = 
        let drawAt x =
            Console.BackgroundColor <- col
            Console.SetCursorPosition (x, y)
            Console.Write(" ")
            Console.BackgroundColor <- ConsoleColor.Black
        drawAt (2*x)
        drawAt (2*x+1)

    let drawPiece x y piece = 
        let cells = cellsOccupiedByPiece piece
        match piece with
        | Piece (_, _, _, color) -> List.iter (fun pos -> drawCell (x + pos.x) (x + pos.y) color) cells

    let rec private drawBoard x y cells =
        let drawCell x y c =
            match c with
            | Some col -> drawCell x y col
            | None     -> drawCell x y ConsoleColor.Black
        let rec drawLine x y l =
            match l with
            | (c::cs) -> drawCell x y c ; drawLine (x+1) y cs 
            | _       -> () 
        match cells with
        | (l::ls) -> drawLine x y l ; drawBoard x (y+1) ls
        | _       -> ()

    let private drawHintArea x y (Piece (_, _, shape, color)) = 
        let hintPiece = Piece (2, 4, shape, color)
        let cells = cellsOccupiedByPiece hintPiece
        List.iter (fun y' -> List.iter (fun x' -> drawCell (x+x') (y+y') ConsoleColor.Black) [0;1;2;3]) [0;1;2;3]
        drawPiece x y hintPiece

    let drawText x y col (text: string) =
        let oldColor = Console.ForegroundColor  
        Console.ForegroundColor <- col
        Console.SetCursorPosition (x, y)
        Console.Write(text)
        Console.ForegroundColor <- oldColor

    let drawFrame x y w h =
        List.init w (fun i -> i + x) |> List.iter (fun x -> drawCell x y ConsoleColor.DarkGray)
        List.init h (fun i -> i + y) |> List.iter (fun y -> drawCell x y ConsoleColor.DarkGray)
        List.init h (fun i -> i + y) |> List.iter (fun y -> drawCell (x+w-1) y ConsoleColor.DarkGray)
        List.init w (fun i -> i + x) |> List.iter (fun x -> drawCell x (y+h-1) ConsoleColor.DarkGray)

    let drawState (State (Board cells, pieceOpt, nextPiece, lines, score)) =
        let textLeft = boardLeft + boardWidth * 2 + textOffset
        let hintLeft = textLeft / 2
        drawBoard boardLeft boardTop cells
        match pieceOpt with
        | Some piece -> drawPiece boardLeft boardTop piece
        | None       -> ()
        drawFrame (boardLeft-1) (boardTop-1) (boardWidth+2) (boardHeight+2)
        drawText textLeft (boardTop+1) ConsoleColor.Gray (sprintf "Level: %d          " (getLevel score))
        drawText textLeft (boardTop+3) ConsoleColor.Gray (sprintf "Lines: %d          " lines)
        drawText textLeft (boardTop+5) ConsoleColor.Gray (sprintf "Score: %d          " score)
        drawText textLeft (boardTop+boardHeight-HintHeight-3) ConsoleColor.Gray "Next piece:"
        drawFrame hintLeft (boardTop+boardHeight-HintHeight-1) 6 6
        drawHintArea (hintLeft+1) (boardTop+boardHeight-HintHeight) nextPiece
