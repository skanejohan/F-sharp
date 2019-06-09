module BoardFunctions

    open System
    open DomainTypes

    // Helpers

    let private getLine = List.init boardWidth (fun _ -> None)

    let private getLines n = List.init n (fun _ -> getLine)

    // Read from the board
    
    let isOccupied x y (Board cells) = Option.isSome cells.[y].[x]

    let isValid x y = x >= 0 && y >= 0 && x < boardWidth && y < boardHeight

    let isAvailable x y board = isValid x y && not (isOccupied x y board)

    let isAboveObstacle x y board = isValid x y && (y = boardHeight - 1 || isOccupied x (y+1) board)

    // Return a (modified) board

    let set x y c (Board cells) = 
        let setX cs =
            let cs1 = Seq.toList <| Seq.take x cs
            let cs2 = Seq.toList <| Seq.skip (x+1) cs
            List.concat [cs1 ; [Some c] ; cs2]
        let l1 = Seq.toList <| Seq.take y cells
        let line = setX cells.[y]
        let l2 = Seq.toList (Seq.skip (y+1) cells)
        Board (List.concat [l1 ; [line] ; l2])

    let removeFullLines (Board lines) = 
        let isFull line = List.forall Option.isSome line 
        let remainingLines = List.filter (fun line -> not (isFull line)) lines   
        let noOfRemovedLines = boardHeight - List.length remainingLines 
        (noOfRemovedLines, Board (List.append (getLines noOfRemovedLines) remainingLines))

    let createBoard = Board (getLines boardHeight)
