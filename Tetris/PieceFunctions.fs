module PieceFunctions

    open System
    open DomainTypes

    // Read from the piece

    let cellsOccupiedByPiece (Piece (cx, cy, shape, _)) = List.map (fun c -> { x = cx + c.x; y = cy + c.y}) shape

    // Return a (modified) piece

    let rotatePiece (Piece (cx, cy, shape, color)) = 
        let rotations = Map.ofList [ 
            ({ x = -1; y =  1 }, { x = -1; y = -1 })
            ({ x =  0; y =  1 }, { x = -1; y =  0 })
            ({ x =  1; y =  1 }, { x = -1; y =  1 })
            ({ x = -2; y =  0 }, { x =  0; y = -2 })
            ({ x = -1; y =  0 }, { x =  0; y = -1 })
            ({ x =  0; y =  0 }, { x =  0; y =  0 })
            ({ x =  1; y =  0 }, { x =  0; y =  1 })
            ({ x = -1; y = -1 }, { x =  1; y = -1 })
            ({ x =  0; y = -1 }, { x =  1; y =  0 })
            ({ x =  1; y = -1 }, { x =  1; y =  1 })
            ({ x =  0; y = -2 }, { x =  2; y =  0 })
            ({ x =  2; y =  0 }, { x =  0; y =  2 })
            ({ x =  0; y =  2 }, { x = -2; y =  0 })
        ]
        Piece (cx, cy, List.map (fun c -> rotations.Item(c)) shape, color)

    let translatePiece dx dy (Piece (cx, cy, shape, color)) = Piece (cx + dx, cy + dy, shape, color)

    let private O = ([{ x = -1; y =  0 }; { x =  0; y =  0 }; { x = -1; y = -1 }; { x =  0; y = -1 }], ConsoleColor.Yellow)
    let private I = ([{ x = -2; y =  0 }; { x = -1; y =  0 }; { x =  0; y =  0 }; { x =  1; y =  0 }], ConsoleColor.Cyan)
    let private S = ([{ x =  0; y =  0 }; { x =  1; y =  0 }; { x = -1; y = -1 }; { x =  0; y = -1 }], ConsoleColor.Green)
    let private Z = ([{ x = -1; y =  0 }; { x =  0; y =  0 }; { x =  0; y = -1 }; { x =  1; y = -1 }], ConsoleColor.Red)
    let private L = ([{ x = -1; y =  0 }; { x =  0; y =  0 }; { x =  1; y =  0 }; { x = -1; y = -1 }], ConsoleColor.DarkYellow)
    let private J = ([{ x = -1; y =  0 }; { x =  0; y =  0 }; { x =  1; y =  0 }; { x =  1; y = -1 }], ConsoleColor.Blue)
    let private T = ([{ x = -1; y =  0 }; { x =  0; y =  0 }; { x =  1; y =  0 }; { x =  0; y = -1 }], ConsoleColor.Magenta)
    let private pieces = [O; I; S; Z; L; J; T]

    let private rnd = Random ()

    let createPiece () = 
        let piece = pieces.[rnd.Next(List.length pieces)]
        Piece(5, 0, fst piece, snd piece)