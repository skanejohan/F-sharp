module StateFunctions

    open DomainTypes
    open PieceFunctions
    open BoardFunctions

    let getLevel score = 1 + score / 100

    let private canPlacePiece board piece = List.forall (fun pos -> isAvailable pos.x pos.y board) (cellsOccupiedByPiece piece)

    let private pieceHasLanded board (Piece (x, y, shape, color) as piece) = List.exists (fun pos -> isAboveObstacle pos.x pos.y board) (cellsOccupiedByPiece piece)

    let private transferPiece board (Piece (x, y, shape, color) as piece) = List.fold (fun b c -> set c.x c.y color b) board (cellsOccupiedByPiece piece)

    let private updateScore score level removedLines quickdrop = 
        let sc1 = level * 10 * removedLines
        let sc2 = if removedLines > 3 then level * 10 else 0 // Extra bonus for four completed rows
        let sc3 = if quickdrop then level * 10 else 0 // Extra bonus for drop
        score + sc1 + sc2 + sc3

    let private updateLines lines removedLines = lines + removedLines 

    let private apply pieceTransformer state =
        match state with
        | State (board, Some piece, nextPiece, lines, score) ->
            let transformedPiece = pieceTransformer piece
            if canPlacePiece board transformedPiece then
                if pieceHasLanded board transformedPiece then
                    let (removedLines, newBoard) = transferPiece board transformedPiece |> removeFullLines
                    (true, true, State (newBoard, Some nextPiece, createPiece (), updateLines lines removedLines, updateScore score (getLevel score) removedLines false))
                else
                    (true, false, State (board, Some transformedPiece, nextPiece, lines, score))
            else 
                (false, false, state)
        | _ -> (false, false, state)

    let initial state = (true, false, state)

    let id state = (false, false, state)

    let moveLeft = apply (translatePiece -1 0)

    let moveDown = apply (translatePiece 0 1)

    let moveRight = apply (translatePiece 1 0)

    let rotate = apply rotatePiece

    let rec drop state = 
        match apply (translatePiece 0 1) state with
        | (true, false, newState) -> drop newState
        | (true, true, newState) -> (true, true, newState)
        | (false, _, state) -> (true, false, state) 

    let createState = State (createBoard, Some (createPiece ()), createPiece (), 0, 0)
